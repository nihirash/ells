package com.nihirash.ells

import fastparse.all._

object Parser {
  val numberParser: Parser[EllsNumber] = P(doubleParser | longsParser)
  val identityParser: Parser[EllsIdentifier] = P(((charParser | specialCharsParser) ~ (digitsParser | specialCharsParser | charParser).rep).!.map(id => EllsIdentifier(id)))
  val stringParser: Parser[EllsString] = P((sepparatorParser ~ "\"" ~/ (strCharsParser | escapeParser).rep.! ~ "\"").map(str => EllsString(StringContext.treatEscapes(str))))
  val booleanParser: Parser[EllsBoolean] = P(("true" | "false").!.map {
    case "true" => EllsBoolean(true)
    case _ => EllsBoolean(false)
  })

  private val sepparatorParser = P(CharsWhileIn(" \r\n\t,").?)
  private val specialCharsParser = P(CharsWhileIn("!@#$%^&*_-><="))
  private val digitsParser = P(CharsWhileIn("0123456789"))
  private val fractionalParser = P("." ~ digitsParser)
  private val exponentParser = P(CharIn("eE") ~ CharIn("+-").? ~ digitsParser)
  private val longsParser = P(("-".? ~ digitsParser ~ sepparatorParser).!.map(l => EllsLong(l.toLong)))
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
