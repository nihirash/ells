package com.nihirash.ells

trait EllsType

sealed trait EllsNumber extends EllsType with Ordered[EllsNumber] {
  def toDouble: Double

  def toLong: Long
}

case class EllsNil() extends EllsType

case class EllsList(v: List[EllsType]) extends EllsType {
  override def toString: String = "(" + v.map(_.toString).mkString(" ") + ")"
}

case class EllsDouble(v: Double) extends EllsNumber {
  override def toDouble: Double = v

  override def toLong: Long = v.toLong

  override def toString: String = v.toString

  override def compare(that: EllsNumber): Int = v.compareTo(that.toDouble)
}

case class EllsLong(v: Long) extends EllsNumber {
  override def toDouble: Double = v.toDouble

  override def toLong: Long = v

  override def toString: String = v.toString

  override def compare(that: EllsNumber): Int = v.compareTo(that.toLong)
}


case class EllBoolean(v: Boolean) extends EllsType {
  override def toString: String = v.toString
}

case class EllsString(v: String) extends EllsType {
  override def toString: String = v
}

case class EllsIdentifier(v: String) extends EllsType {
  override def toString: String = v
}

case class EllsBoolean(b: Boolean) extends EllsType {
  override def toString: String = if (b) "true" else "false"
}
