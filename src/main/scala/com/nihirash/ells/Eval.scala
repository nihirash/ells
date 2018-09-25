package com.nihirash.ells

import scala.annotation.tailrec
import scala.collection.mutable.{Map => MutableMap}

class Eval {
  type Args = List[EllsType]

  def eval(exprs: Seq[EllsType], env: Env = Env.empty): EllsType = exprs.map(e => evalExpression(e, env)).last

  def evalExpression(e: EllsType, env: Env): EllsType = e match {
    case e: EllsScalar => e
    case l: EllsList => evalForm(l, env)
    case i: EllsIdentifier => env.get(i)
    case _ => throw new Exception(s"Can't eval $e")
  }

  private def evalForm(l: EllsList, env: Env): EllsType = {
    val car = l.v.head
    val cdr = l.v.tail
    car match {
      case i: EllsIdentifier => evalCall(i, cdr, env)
      case l: EllsFunction => evalFunction(l, cdr, env)
      case f => throw new RuntimeException(s"Cant eval form: $l")
    }
  }

  private def evalFunction(fn: EllsFunction, args: Args, env: Env): EllsType = {
    if (fn.args.length != args.length) throw EllsArityException()
    val argValues = fn.args zip args
    val innerEnv = Env(
      parent = Some(env),
      definitions = MutableMap(argValues: _*)
    )
    eval(fn.body, innerEnv)
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
    case "fn" => fnForm(args, env)
    case "defn" => defnForm(args, env)
    case _ => env.get(id) match {
      case EllsNil() => EllsNil()
      case f: EllsFunction => evalFunction(f, args.map(evalExpression(_, env)), env)
      case _ => throw EllsEvalException(s"Can't eval form '$id' with args '$args'")
    }

  }

  private def fnForm(args: Args, env: Env): EllsType = {
    val funArgs = args.head
    funArgs match {
      case EllsNil() => EllsFunction(List.empty, args.tail)
      case EllsList(l) => EllsFunction(l.map(_.toIdentifier), args.tail)
      case _ => throw EllsEvalException(s"wrong function form (fn $args)")
    }
  }

  private def defnForm(args: Args, env: Env): EllsType =
    defForm(List(args.head, EllsList(EllsIdentifier("fn") +: args.tail)), env)

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
      case EllsNil() => EllsList(args.tail)
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