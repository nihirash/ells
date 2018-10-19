package com.nihirash.ells.lib
import com.nihirash.ells._

import scala.collection.mutable.{Map => MutableMap}
import scala.annotation.tailrec

class BaseForms(val eval: (EllsType, Env) => EllsType) extends SpecialForm {
  import Implicits._

  override def call(id: EllsIdentifier, args: List[EllsType], env: Env): Option[EllsType] =
    Option(id.v match {
      case "quote"       => quote(args, env)
      case "eval"        => eval(args.map(a => eval(a, env)).last, env)
      case "do"          => args.map(eval(_, env)).last
      case "if"          => ifForm(args, env)
      case "and"         => andForm(args, env)
      case "or"          => orForm(args, env)
      case "not"         => notForm(args, env)
      case "def"         => defForm(args, env)
      case "set"         => setForm(args, env)
      case "fn"          => fnForm(args, env)
      case "is-nil?"     => isNilForm(args, env)
      case "is-number?"  => isNumberForm(args, env)
      case "is-string?"  => isString(args, env)
      case "is-boolean?" => isBoolean(args, env)
      case "is-fun?"     => isFun(args, env)
      case "is-list?"    => isList(args, env)
      case "to-string"   => toStringForm(args, env)
      case "to-number"   => toNumberForm(args, env)
      case "to-long"     => toLong(args, env)
      case "defn"        => defnForm(args, env)
      case "throw"       => throwForm(args, env)
      case "try"         => tryForm(args, env)
      case _             => null
    })

  private def quote(tail: List[EllsType], env: Env): EllsType = tail match {
    case h :: Nil => unquote(h, env)
    case h :: t   => throw EllsArityException()
    case Nil      => throw EllsArityException()
  }

  private def unquote(arg: EllsType, env: Env): EllsType = arg match {
    case EllsList(List(EllsIdentifier("unquote"), v)) => eval(v, env)
    case EllsList(v)                                  => EllsList(v.map(unquote(_, env)))
    case t: EllsType                                  => t
  }

  private def toLong(args: List[EllsType], env: Env): EllsType =
    toNumberForm(args, env).toNumber.toLong

  private def tryForm(args: List[EllsType], env: Env): EllsType = args match {
    case tryBlock :: catchBlock :: Nil =>
      try {
        eval(tryBlock, env)
      } catch {
        case e: Throwable =>
          val innerEnv =
            Env(parent = Some(env), MutableMap(EllsIdentifier("exception") -> e.getLocalizedMessage))
          eval(catchBlock, innerEnv)
      }
    case _ => throw EllsArityException()
  }

  private def throwForm(args: List[EllsType], env: Env): EllsType = args match {
    case arg :: Nil => throw EllsRuntimeException(arg.toString)
    case _          => throw EllsArityException()
  }

  private def toNumberForm(args: List[EllsType], env: Env): EllsType = args match {
    case arg :: Nil => eval(arg, env).toString.toDouble
    case _          => throw EllsArityException()
  }

  private def toStringForm(args: List[EllsType], env: Env): EllsType = args match {
    case arg :: Nil => eval(arg, env).toString
    case _          => throw EllsArityException()
  }

  private def isList(args: List[EllsType], env: Env): EllsType = args match {
    case arg :: Nil =>
      eval(arg, env) match {
        case _: EllsList => true
        case EllsNil()   => true
        case _           => false
      }
    case _ => throw EllsArityException()
  }

  private def isFun(args: List[EllsType], env: Env): EllsType = args match {
    case arg :: Nil =>
      eval(arg, env) match {
        case EllsFunction(_, _) => true
        case _                  => false
      }
    case _ => throw EllsArityException()
  }

  private def isString(args: List[EllsType], env: Env): EllsType = args match {
    case arg :: Nil =>
      eval(arg, env) match {
        case _: EllsString => true
        case _             => false
      }
    case _ => throw EllsArityException()
  }

  private def isBoolean(args: List[EllsType], env: Env): EllsType = args match {
    case arg :: Nil =>
      arg match {
        case _: EllsBoolean => true
        case _              => false
      }
    case _ => throw EllsArityException()
  }

  private def isNilForm(args: List[EllsType], env: Env): EllsType = args match {
    case arg :: Nil => eval(arg, env).isNil
    case _          => throw EllsArityException()
  }

  private def isNumberForm(args: List[EllsType], env: Env): EllsType =
    args match {
      case arg :: Nil =>
        eval(arg, env) match {
          case _: EllsNumber => true
          case _             => false
        }
      case _ => throw EllsArityException()
    }

  private def fnForm(args: List[EllsType], env: Env): EllsType = {
    val funArgs = args.head
    funArgs match {
      case EllsNil()   => EllsFunction(List.empty, args.tail)
      case EllsList(l) => EllsFunction(l.map(_.toIdentifier), args.tail)
      case _           => throw EllsEvalException(s"wrong function form (fn $args)")
    }
  }

  private def defnForm(args: List[EllsType], env: Env): EllsType =
    defForm(List(args.head, EllsList(EllsIdentifier("fn") +: args.tail)), env)

  private def defForm(args: List[EllsType], env: Env): EllsType = {
    val id = args.head
    id match {
      case id: EllsIdentifier =>
        args.tail match {
          case value :: Nil =>
            env.define(id, eval(value, env))
            value
          case _ => throw EllsArityException("Expected one expression")
        }
      case _ => throw EllsTypesException("Expected identifier")
    }
  }

  private def setForm(args: List[EllsType], env: Env): EllsType = {
    val id = args.head
    id match {
      case id: EllsIdentifier =>
        args.tail match {
          case value :: Nil =>
            env.set(id, eval(value, env))
            value
          case _ => throw EllsArityException("Expected one expression")
        }
      case _ => throw EllsTypesException("Expected identifier")
    }
  }

  private def ifForm(tail: List[EllsType], env: Env): EllsType =
    tail match {
      case expression :: rightCase :: leftCase :: Nil =>
        if (!eval(expression, env).isNil)
          eval(rightCase, env)
        else
          eval(leftCase, env)
      case expression :: rightCase :: Nil =>
        if (!eval(expression, env).isNil)
          eval(rightCase, env)
        else
          EllsNil()
      case _ => throw EllsArityException("Wrong IF-form")
    }

  @tailrec
  private def andForm(args: List[EllsType], env: Env, acc: Boolean = false): EllsType = {
    args match {
      case h :: t => andForm(t, env, !eval(h, env).isNil)
      case Nil    => acc
    }
  }

  @tailrec
  private def orForm(args: List[EllsType], env: Env): EllsType = args match {
    case Nil => false
    case h :: t =>
      val headNil = eval(h, env).isNil
      if (headNil) orForm(t, env) else true
  }

  private def notForm(args: List[EllsType], env: Env): EllsType = args match {
    case h :: Nil => eval(h, env).isNil
    case _        => throw EllsArityException("Expected one expression")
  }
}
