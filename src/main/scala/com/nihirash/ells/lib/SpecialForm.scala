package com.nihirash.ells.lib
import com.nihirash.ells.{EllsIdentifier, EllsType, Env}

trait SpecialForm {
  val eval: (EllsType, Env) => EllsType

  def call(id: EllsIdentifier, args: List[EllsType], env: Env): Option[EllsType]
}
