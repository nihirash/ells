package com.nihirash.ells.lib
import com.nihirash.ells._

class ListForms(val eval: (EllsType, Env) => EllsType) extends SpecialForm {
  import Implicits._

  override def call(id: EllsIdentifier, args: List[EllsType], env: Env): Option[EllsType] =
    Option(id.v match {
      case "list"        => EllsList(args.map(eval(_, env)))
      case "head"        => listHead(args, env)
      case "tail"        => listTail(args, env)
      case "list-append" => listAppend(args, env)
      case "merge-lists" => listMerge(args, env)
      case _             => null
    })

  private def listMerge(args: List[EllsType], env: Env): EllsType = {
    args.map(eval(_, env)) match {
      case EllsList(first) :: EllsList(second) :: Nil =>
        first ++ second
      case _ => throw EllsArityException("Only two list are acceptable")
    }
  }

  private def listHead(tail: List[EllsType], env: Env): EllsType = {
    tail.map(eval(_, env)) match {
      case v :: Nil =>
        v match {
          case EllsList(l) => l.head
          case EllsNil()   => EllsNil()
          case _           => throw EllsTypesException("Only list are acceptable")
        }
      case _ => throw EllsArityException("Only one list are acceptable")
    }
  }

  private def listTail(tail: List[EllsType], env: Env): EllsType = tail.map(eval(_, env)) match {
    case v :: Nil =>
      v match {
        case EllsList(l) => l.tail
        case _           => throw EllsTypesException("Only non-empty lists are acceptable")
      }
    case _ => throw EllsArityException("Only one list are acceptable")
  }

  private def listAppend(tail: List[EllsType], env: Env): EllsType = {
    val args = tail.map(eval(_, env))
    val head = args.head
    head match {
      case EllsNil()   => args.tail
      case EllsList(v) => v ++ args.tail
      case v: EllsType => v +: args.tail
    }
  }
}
