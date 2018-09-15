package com.nihirash.ells

class Eval {
  type Args = List[EllsType]

  def eval(exprs: Seq[EllsType]): EllsType = exprs.map(evalExpression).last

  def evalExpression(e: EllsType): EllsType = e match {
    case e: EllsScalar => e
    case l: EllsList => evalForm(l)
    case _ => throw new Exception("Can't eval")
  }

  private def evalForm(l: EllsList): EllsType = {
    val car = l.v.head
    val cdr = l.v.tail
    car match {
      case i: EllsIdentifier => evalCall(i, cdr)
      case f => throw new RuntimeException(s"Cant eval form: $l")
    }
  }

  private def evalCall(id: EllsIdentifier, args: Args): EllsType = id.v match {
    case "quote" => quote(args)
    case "+" => plus(args)
    case "-" => minus(args)
    case "*" => mult(args)
    case "/" => divide(args)
    case "list" => EllsList(args.map(evalExpression))
    case "head" => listHead(args)
    case "tail" => listTail(args)
    case "min" => listMin(args)
    case "max" => listMax(args)
    case ">" => more(args)
    case "<" => less(args)
    case "=" => isEqual(args)
    case "do" => eval(args)
    case "if" => ifForm(args)
    case f => throw EllsEvalException(s"Can't eval '$f' form")
  }

  private def quote(tail: Args): EllsType = tail match {
    case h :: Nil => h
    case h :: t => throw EllsArityException()
    case Nil => throw EllsArityException()
  }

  private def plus(tail: Args): EllsType = {
    val args = tail.map(v => evalExpression(v).toNumber)
    args.tail.fold(args.head)((l: EllsNumber, r: EllsNumber) => l + r)
  }

  private def minus(tail: Args): EllsType = {
    val args = tail.map(v => evalExpression(v).toNumber)
    args.tail.fold(args.head)((l: EllsNumber, r: EllsNumber) => l - r)
  }

  private def mult(tail: Args): EllsType = {
    val args = tail.map(v => evalExpression(v).toNumber)
    args.tail.fold(args.head)((l: EllsNumber, r: EllsNumber) => l * r)
  }

  private def divide(tail: Args): EllsType = {
    val args = tail.map(v => evalExpression(v).toNumber)
    args.tail.fold(args.head)((l: EllsNumber, r: EllsNumber) => if (!r.isNil) l / r else throw new ArithmeticException("Division by zero"))
  }

  private def listHead(tail: Args): EllsType = {
    tail.map(evalExpression) match {
      case v :: Nil => v match {
        case EllsList(l) => l.head
        case _ => throw EllsTypesException("Only list are acceptable")
      }
      case _ => throw EllsArityException("Only one list are acceptable")
    }
  }

  private def listTail(tail: Args): EllsType = tail.map(evalExpression) match {
    case v :: Nil => v match {
      case EllsList(l) => EllsList(l.tail)
      case _ => throw EllsTypesException("Only non-empty lists are acceptable")
    }
    case _ => throw EllsArityException("Only one list are acceptable")
  }


  private def listMin(tail: Args): EllsType = tail.map(evalExpression) match {
    case x :: Nil => x match {
      case EllsList(l) => l.map(_.toNumber).min
      case _ => throw EllsTypesException("Only numeric list are acceptable")
    }
    case _ => throw EllsArityException("Only one lisp acceptable")
  }

  private def listMax(tail: Args): EllsType = tail.map(evalExpression) match {
    case x :: Nil => x match {
      case EllsList(l) => l.map(_.toNumber).max
      case _ => throw EllsTypesException("Only numeric list are acceptable")
    }
    case _ => throw EllsArityException("Only one lisp acceptable")
  }

  private def more(tail: Args): EllsType =
    tail.map(evalExpression(_).toNumber) match {
      case car :: cdr :: Nil => EllsBoolean(car > cdr)
      case _ => throw EllsArityException("This is binary operation")
    }

  private def less(tail: Args): EllsType =
    tail.map(evalExpression(_).toNumber) match {
      case car :: cdr :: Nil => EllsBoolean(car < cdr)
      case _ => throw EllsArityException("This is binary operation")
    }

  private def isEqual(tail: Args): EllsType =
    tail.map(evalExpression) match {
      case car :: cdr :: Nil => EllsBoolean(car == cdr)
      case _ => throw EllsArityException("This is binary operation")
    }

  private def ifForm(tail: Args): EllsType =
    tail match {
      case expression :: rightCase :: leftCase :: Nil =>
        if (!evalExpression(expression).isNil)
          evalExpression(rightCase)
        else
          evalExpression(leftCase)
      case expression :: rightCase :: Nil =>
        if (!evalExpression(expression).isNil)
          evalExpression(rightCase)
        else
          EllsNil()
      case _ => throw EllsArityException("Wrong IF-form")
    }
}