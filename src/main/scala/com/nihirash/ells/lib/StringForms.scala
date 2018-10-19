package com.nihirash.ells.lib
import com.nihirash.ells._

class StringForms(val eval: (EllsType, Env) => EllsType) extends SpecialForm {
  import Implicits._

  override def call(id: EllsIdentifier, args: List[EllsType], env: Env): Option[EllsType] =
    Option(id.v match {
      case "string-length" => stringLength(args, env)
      case "substring"     => substringForm(args, env)
      case "string-append" => stringAppend(args, env)
      case "split-string"  => splitString(args, env)
      case _               => null
    })

  private def splitString(args: List[EllsType], env: Env): EllsType = args match {
    case str :: sepparator :: Nil =>
      EllsList(
        eval(str, env).toString
          .split(eval(sepparator, env).toString)
          .map(EllsString)
          .toList
      )
    case _ => throw EllsArityException()
  }

  private def stringAppend(args: List[EllsType], env: Env): EllsType =
    args.map(eval(_, env).toString).mkString("")

  private def substringForm(args: List[EllsType], env: Env): EllsType = args match {
    case str :: begin :: length :: Nil =>
      val strBegin = begin.toNumber.toLong.toInt
      val strEnd = strBegin + length.toNumber.toLong.toInt
      str.toString.substring(strBegin, strEnd)

    case _ => throw EllsArityException()
  }

  private def stringLength(args: List[EllsType], env: Env): EllsType = args match {
    case arg :: Nil => eval(arg, env).toString.length
    case _          => throw EllsArityException()
  }

}
