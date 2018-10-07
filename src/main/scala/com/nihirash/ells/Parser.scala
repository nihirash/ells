package com.nihirash.ells

import fastparse.all._

object Parser {
  type parseResult = Either[String, Seq[EllsType]]
  val expressionParser: Parser[EllsType] = P(nilParser | numberParser | stringParser | booleanParser | identityParser | listParser | quoteParser | unquoteParser)

  val numberParser: Parser[EllsType] = P(doubleParser | longsParser)
  val identityParser: Parser[EllsType] = P(((charParser | specialCharsParser) ~ (digitsParser | specialCharsParser | charParser).rep).!.map(id => EllsIdentifier(id)))
  val stringParser: Parser[EllsType] = P((separatorParser ~ "\"" ~/ (strCharsParser | escapeParser).rep.! ~ "\"").map(str => EllsString(StringContext.treatEscapes(str))))
  val booleanParser: Parser[EllsType] = P(("true" | "false").!.map {
    case "true" => EllsBoolean(true)
    case _ => EllsBoolean(false)
  })
  val nilParser: Parser[EllsType] = P(("nil" | "()").!.map(_ => EllsNil()))
  val listParser: Parser[EllsType] = P(("(" ~/ expressionParser.rep(sep = separatorParser) ~ separatorParser ~ ")")).map(v => EllsList(v.toList))
  val bodyParser: Parser[Seq[EllsType]] = P(separatorParser ~ expressionParser.rep(sep = separatorParser))
  private val specialCharsParser = P(CharsWhileIn("!#$%^&*_-><=+*/"))

  private val quoteParser: Parser[EllsType] = P("'" ~ expressionParser).map(v => EllsList(List(EllsIdentifier("quote"), v)))
  private val unquoteParser: Parser[EllsType] = P("@" ~ expressionParser).map(v => EllsList(List(EllsIdentifier("unquote"), v)))

  private val separatorParser = P(CharsWhileIn(" \r\n\t,").?)

  def apply(s: String): parseResult = bodyParser.parse(s)
    .fold(
      (_, index, x) => Left(s"Parse failed at position $index. Expected ${x.traced.expected}"),
      (r, _) => Right(r)
    )
  private val digitsParser = P(CharsWhileIn("0123456789"))
  private val fractionalParser = P("." ~ digitsParser)
  private val exponentParser = P(CharIn("eE") ~ CharIn("+-").? ~ digitsParser)
  private val longsParser = P(("-".? ~ digitsParser).!.map(l => EllsLong(l.toLong)))
  private val doubleParser: P[EllsDouble] = P(("-".? ~ (digitsParser ~ fractionalParser ~ exponentParser.?)).!.map(d => EllsDouble(d.toDouble)))
  private val charParser = P(CharIn('A' to 'Z') | CharIn('a' to 'z'))
  private val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")
  private val escapeParser = P("\\" ~ (CharIn("\"/\\bfnrt")))
  private val strCharsParser = P(CharsWhile(StringChars))

}

case class NamedFunction[T, V](f: T => V, name: String) extends (T => V) {
  def apply(t: T) = f(t)

  override def toString() = name
}
