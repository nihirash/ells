package com.nihirash.ells

import scala.annotation.tailrec

class Eval {
  type Args = List[EllsType]

  def eval(exprs: Seq[EllsType], env: Env = Env.empty): EllsType = exprs.map(e => evalExpression(e, env)).last

  def evalExpression(e: EllsType, env: Env): EllsType = e match {
    case e: EllsScalar => e
    case l: EllsList => evalForm(l, env)
    case i: EllsIdentifier => env.get(i)
    case _ => throw new Exception("Can't eval")
  }

  private def evalForm(l: EllsList, env: Env): EllsType = {
    val car = l.v.head
    val cdr = l.v.tail
    car match {
      case i: EllsIdentifier => evalCall(i, cdr, env)
      case f => throw new RuntimeException(s"Cant eval form: $l")
    }
  }

  private def evalCall(id: EllsIdentifier, args: Args, env: Env): EllsType = id.v match {
    case "quote" => quote(args)
    case "+" => plus(args, env)
    case "-" => minus(args, env)
    case "*" => mult(args, env)
    case "/" => divide(args, env)
    case "list" => EllsList(args.map(evalExpression(_, env)))
    case "head" => listHead(args, env)
    case "tail" => listTail(args, env)
    case "list-append" => listAppend(args, env)
    case "min" => listMin(args, env)
    case "max" => listMax(args, env)
    case ">" => more(args, env)
    case "<" => less(args, env)
    case "=" => isEqual(args, env)
    case "do" => eval(args, env)
    case "if" => ifForm(args, env)
    case "and" => andForm(args, env)
    case "or" => orForm(args, env)
    case "not" => notForm(args, env)
    case "def" => defForm(args, env)
    case "set" => setForm(args, env)
    case f => throw EllsEvalException(s"Can't eval '$f' form")
  }

  private def defForm(args: Args, env: Env): EllsType = {
    val id = args.head
    id match {
      case id: EllsIdentifier =>
        args.tail match {
          case value :: Nil =>
            env.define(id, evalExpression(value, env))
            value
          case _ => throw EllsArityException("Expected one expression")
        }
      case _ => throw EllsTypesException("Expected identifier")
    }
  }

  private def setForm(args: Args, env: Env): EllsType = {
    val id = args.head
    id match {
      case id: EllsIdentifier =>
        args.tail match {
          case value :: Nil =>
            env.set(id, evalExpression(value, env))
            value
          case _ => throw EllsArityException("Expected one expression")
        }
      case _ => throw EllsTypesException("Expected identifier")
    }
  }


  private def quote(tail: Args): EllsType = tail match {
    case h :: Nil => h
    case h :: t => throw EllsArityException()
    case Nil => throw EllsArityException()
  }

  private def plus(tail: Args, env: Env): EllsType = {
    val args = tail.map(v => evalExpression(v, env).toNumber)
    args.tail.fold(args.head)((l: EllsNumber, r: EllsNumber) => l + r)
  }

  private def minus(tail: Args, env: Env): EllsType = {
    val args = tail.map(v => evalExpression(v, env).toNumber)
    args.tail.fold(args.head)((l: EllsNumber, r: EllsNumber) => l - r)
  }

  private def mult(tail: Args, env: Env): EllsType = {
    val args = tail.map(v => evalExpression(v, env).toNumber)
    args.tail.fold(args.head)((l: EllsNumber, r: EllsNumber) => l * r)
  }

  private def divide(tail: Args, env: Env): EllsType = {
    val args = tail.map(v => evalExpression(v, env).toNumber)
    args.tail.fold(args.head)((l: EllsNumber, r: EllsNumber) => if (!r.isNil) l / r else throw new ArithmeticException("Division by zero"))
  }

  private def listHead(tail: Args, env: Env): EllsType = {
    tail.map(evalExpression(_, env)) match {
      case v :: Nil => v match {
        case EllsList(l) => l.head
        case EllsNil() => EllsNil()
        case _ => throw EllsTypesException("Only list are acceptable")
      }
      case _ => throw EllsArityException("Only one list are acceptable")
    }
  }

  private def listTail(tail: Args, env: Env): EllsType = tail.map(evalExpression(_, env)) match {
    case v :: Nil => v match {
      case EllsList(l) => EllsList(l.tail)
      case _ => throw EllsTypesException("Only non-empty lists are acceptable")
    }
    case _ => throw EllsArityException("Only one list are acceptable")
  }

  private def listAppend(tail: Args, env: Env): EllsType = {
    val args = tail.map(evalExpression(_, env))
    val head = args.head
    head match {
      case EllsList(v) => EllsList(v ++ args.tail)
      case v: EllsType => EllsList(v +: args.tail)
    }
  }


  private def listMin(tail: Args, env: Env): EllsType = tail.map(evalExpression(_, env)) match {
    case x :: Nil => x match {
      case EllsList(l) => l.map(_.toNumber).min
      case _ => throw EllsTypesException("Only numeric list are acceptable")
    }
    case _ => throw EllsArityException("Only one lisp acceptable")
  }

  private def listMax(tail: Args, env: Env): EllsType = tail.map(evalExpression(_, env)) match {
    case x :: Nil => x match {
      case EllsList(l) => l.map(_.toNumber).max
      case _ => throw EllsTypesException("Only numeric list are acceptable")
    }
    case _ => throw EllsArityException("Only one lisp acceptable")
  }

  private def more(tail: Args, env: Env): EllsType =
    tail.map(evalExpression(_, env).toNumber) match {
      case car :: cdr :: Nil => EllsBoolean(car > cdr)
      case _ => throw EllsArityException("This is binary operation")
    }

  private def less(tail: Args, env: Env): EllsType =
    tail.map(evalExpression(_, env).toNumber) match {
      case car :: cdr :: Nil => EllsBoolean(car < cdr)
      case _ => throw EllsArityException("This is binary operation")
    }

  private def isEqual(tail: Args, env: Env): EllsType =
    tail.map(evalExpression(_, env)) match {
      case car :: cdr :: Nil => EllsBoolean(car == cdr)
      case _ => throw EllsArityException("This is binary operation")
    }

  private def ifForm(tail: Args, env: Env): EllsType =
    tail match {
      case expression :: rightCase :: leftCase :: Nil =>
        if (!evalExpression(expression, env).isNil)
          evalExpression(rightCase, env)
        else
          evalExpression(leftCase, env)
      case expression :: rightCase :: Nil =>
        if (!evalExpression(expression, env).isNil)
          evalExpression(rightCase, env)
        else
          EllsNil()
      case _ => throw EllsArityException("Wrong IF-form")
    }

  @tailrec
  private def andForm(args: Args, env: Env, acc: Boolean = false): EllsType = {
    args match {
      case h :: t => andForm(t, env, !evalExpression(h, env).isNil)
      case Nil => EllsBoolean(acc)
    }
  }

  @tailrec
  private def orForm(args: Args, env: Env): EllsType = args match {
    case Nil => EllsBoolean(false)
    case h :: t =>
      val headNil = evalExpression(h, env).isNil
      if (headNil) orForm(t, env) else EllsBoolean(true)
  }

  private def notForm(args: Args, env: Env): EllsType = args match {
    case h :: Nil => EllsBoolean(evalExpression(h, env).isNil)
    case _ => throw EllsArityException("Expected one expression")
  }
}