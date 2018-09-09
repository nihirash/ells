package com.nihirash.ells

sealed trait EllsType {
  def toNumber: EllsNumber

  def isNil: Boolean
}

sealed trait EllsScalar extends EllsType

sealed trait EllsNumber extends EllsScalar with Ordered[EllsNumber] {
  def toDouble: Double

  def toLong: Long

  def `+`(v2: EllsNumber): EllsNumber

  def `-`(v2: EllsNumber): EllsNumber

  def `/`(v2: EllsNumber): EllsNumber

  def `*`(v2: EllsNumber): EllsNumber

  def empty: EllsNumber

  def toNumber: EllsNumber = this
}

case class EllsNil() extends EllsScalar {
  override def toNumber: EllsNumber = EllsLong(0)

  override def isNil: Boolean = true
}

case class EllsList(v: List[EllsType]) extends EllsType {
  override def toString: String = "(" + v.map(_.toString).mkString(" ") + ")"

  override def isNil: Boolean = v.isEmpty

  override def toNumber: EllsNumber = throw EllsTypesException("Can't cast list to numeric")
}

case class EllsDouble(v: Double) extends EllsNumber {
  override def toDouble: Double = v

  override def toLong: Long = v.toLong

  override def `+`(v2: EllsNumber): EllsNumber = EllsDouble(v + v2.toDouble)

  override def `-`(v2: EllsNumber): EllsNumber = EllsDouble(v - v2.toDouble)

  override def `*`(v2: EllsNumber): EllsNumber = EllsDouble(v * v2.toDouble)

  override def `/`(v2: EllsNumber): EllsNumber = EllsDouble(v / v2.toDouble)

  override def empty: EllsNumber = EllsDouble(0)

  override def isNil: Boolean = v == 0

  override def toString: String = v.toString

  override def compare(that: EllsNumber): Int = v.compareTo(that.toDouble)

  override def equals(o: scala.Any): Boolean = o match {
    case i: EllsNumber => i.toDouble == v.toDouble
    case _ => false
  }
}

case class EllsLong(v: Long) extends EllsNumber {
  override def toDouble: Double = v.toDouble

  override def toLong: Long = v

  override def `+`(v2: EllsNumber): EllsNumber = EllsDouble(v + v2.toLong)

  override def `-`(v2: EllsNumber): EllsNumber = EllsDouble(v - v2.toLong)

  override def `*`(v2: EllsNumber): EllsNumber = EllsDouble(v * v2.toLong)

  override def `/`(v2: EllsNumber): EllsNumber = EllsDouble(v / v2.toLong)

  override def empty: EllsNumber = EllsLong(0)

  override def isNil: Boolean = v == 0

  override def toString: String = v.toString

  override def compare(that: EllsNumber): Int = v.compareTo(that.toLong)

  override def equals(o: scala.Any): Boolean = o match {
    case i: EllsLong => i.toLong == v
    case i: EllsDouble => i.toDouble == toDouble
    case _ => false
  }
}

case class EllsString(v: String) extends EllsScalar {
  override def toString: String = v

  override def toNumber: EllsNumber = throw EllsTypesException("Can't cast string to number")

  override def isNil = v.isEmpty
}

case class EllsIdentifier(v: String) extends EllsScalar {
  override def toString: String = v

  override def toNumber: EllsNumber = throw EllsTypesException("Can't cast identifier to number")

  override def isNil: Boolean = false
}

case class EllsBoolean(b: Boolean) extends EllsScalar {
  override def toString: String = b.toString

  override def toNumber: EllsNumber = if (b) EllsLong(1) else EllsLong(0)

  override def isNil: Boolean = b
}
