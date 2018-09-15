package com.nihirash.ells

class Eval {
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

  private def evalCall(id: EllsIdentifier, tail: List[EllsType]): EllsType = id.v match {
    case "quote" => tail match {
      case h :: Nil => h
      case h :: t => throw EllsArityException()
      case Nil => throw EllsArityException()
    }

    case "+" =>
      val args = tail.map(v => evalExpression(v).toNumber)
      args.tail.fold(args.head)((l: EllsNumber, r: EllsNumber) => l + r)

    case "-" =>
      val args = tail.map(v => evalExpression(v).toNumber)
      args.tail.fold(args.head)((l: EllsNumber, r: EllsNumber) => l - r)

    case "*" =>
      val args = tail.map(v => evalExpression(v).toNumber)
      args.tail.fold(args.head)((l: EllsNumber, r: EllsNumber) => l * r)

    case "/" =>
      val args = tail.map(v => evalExpression(v).toNumber)
      args.tail.fold(args.head)((l: EllsNumber, r: EllsNumber) => if (!r.isNil) l / r else throw new ArithmeticException("Division by zero"))

    case "list" => EllsList(tail.map(evalExpression))

    case "min" => tail.map(evalExpression(_).toNumber).min

    case "max" => tail.map(evalExpression(_).toNumber).max

    case ">" =>
      tail.map(evalExpression(_).toNumber) match {
        case car :: cdr :: Nil => EllsBoolean(car > cdr)
        case _ => throw EllsArityException("This is binary operation")
      }

    case "<" =>
      tail.map(evalExpression(_).toNumber) match {
        case car :: cdr :: Nil => EllsBoolean(car < cdr)
        case _ => throw EllsArityException("This is binary operation")
      }

    case "=" =>
      tail.map(evalExpression) match {
        case car :: cdr :: Nil => EllsBoolean(car == cdr)
        case _ => throw EllsArityException("This is binary operation")
      }

    case "do" => eval(tail)

    case "if" =>
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

    case f => throw EllsEvalException(s"Can't eval '$f' form")
  }
}
