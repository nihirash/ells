package com.nihirash.ells.lib
import com.nihirash.ells._

class MathForms(val eval: (EllsType, Env) => EllsType) extends SpecialForm {
  import Implicits._

  override def call(id: EllsIdentifier, args: List[EllsType], env: Env): Option[EllsType] =
    Option(id.v match {
      case "+"   => plus(args, env)
      case "-"   => minus(args, env)
      case "*"   => mult(args, env)
      case "/"   => divide(args, env)
      case "mod" => mod(args, env)
      case "min" => listMin(args, env)
      case "max" => listMax(args, env)
      case ">"   => more(args, env)
      case "<"   => less(args, env)
      case "="   => isEqual(args, env)
      case _     => null
    })

  private def mod(tail: List[EllsType], env: Env): EllsType = {
    val args = tail.map(eval(_, env).toNumber)
    tail match {
      case arg1 :: arg2 :: Nil => arg1.toNumber.toLong % arg2.toNumber.toLong
      case _ => throw EllsArityException("Only two numeric arguments acceptable")
    }
  }

  private def plus(tail: List[EllsType], env: Env): EllsType = {
    val args = tail.map(eval(_, env).toNumber)
    args.tail.fold(args.head)((l: EllsNumber, r: EllsNumber) => l + r)
  }

  private def minus(tail: List[EllsType], env: Env): EllsType = {
    val args = tail.map(eval(_, env).toNumber)
    args.tail.fold(args.head)((l: EllsNumber, r: EllsNumber) => l - r)
  }

  private def mult(tail: List[EllsType], env: Env): EllsType = {
    val args = tail.map(eval(_, env).toNumber)
    args.tail.fold(args.head)((l: EllsNumber, r: EllsNumber) => l * r)
  }

  private def divide(tail: List[EllsType], env: Env): EllsType = {
    val args = tail.map(eval(_, env).toNumber)
    args.tail.fold(args.head)((l: EllsNumber, r: EllsNumber) =>
      if (!r.isNil) l / r else throw new ArithmeticException("Division by zero"))
  }

  private def listMin(tail: List[EllsType], env: Env): EllsType = tail.map(eval(_, env)) match {
    case x :: Nil =>
      x match {
        case EllsList(l) => l.map(_.toNumber).min
        case _           => throw EllsTypesException("Only numeric list are acceptable")
      }
    case _ => throw EllsArityException("Only one list acceptable")
  }

  private def listMax(tail: List[EllsType], env: Env): EllsType = tail.map(eval(_, env)) match {
    case x :: Nil =>
      x match {
        case EllsList(l) => l.map(_.toNumber).max
        case _           => throw EllsTypesException("Only numeric list are acceptable")
      }
    case _ => throw EllsArityException("Only one list acceptable")
  }

  private def more(tail: List[EllsType], env: Env): EllsType =
    tail.map(eval(_, env).toNumber) match {
      case car :: cdr :: Nil => car > cdr
      case _                 => throw EllsArityException("This is binary operation")
    }

  private def less(tail: List[EllsType], env: Env): EllsType =
    tail.map(eval(_, env).toNumber) match {
      case car :: cdr :: Nil => car < cdr
      case _                 => throw EllsArityException("This is binary operation")
    }

  private def isEqual(tail: List[EllsType], env: Env): EllsType =
    tail.map(eval(_, env)) match {
      case car :: cdr :: Nil => car == cdr
      case _                 => throw EllsArityException("This is binary operation")
    }
}
