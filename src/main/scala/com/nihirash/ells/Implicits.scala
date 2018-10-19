package com.nihirash.ells

object Implicits {
  implicit def long2EllsLong(x: Long): EllsLong = EllsLong(x)
  implicit def int2EllsLong(x: Int): EllsLong = EllsLong(x.toLong)
  implicit def double2EllsDouble(x: Double): EllsDouble = EllsDouble(x)
  implicit def boolean2EllsBoolean(x: Boolean): EllsBoolean = EllsBoolean(x)
  implicit def list2EllsList(x: List[EllsType]): EllsList = EllsList(x)
  implicit def string2EllsString(x: String): EllsString = EllsString(x)
}
