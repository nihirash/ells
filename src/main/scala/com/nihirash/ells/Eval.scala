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
      case _ => throw new RuntimeException("Cant eval form")
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
  }
}
