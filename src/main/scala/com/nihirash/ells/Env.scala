package com.nihirash.ells

import scala.collection.mutable.{Map => MutableMap}

case class Env(parent: Option[Env] = None, definitions: MutableMap[EllsIdentifier, EllsType] = MutableMap.empty) {
  def define(id: EllsIdentifier, value: EllsType): Unit = definitions.update(id, value)

  def get(id: EllsIdentifier): EllsType = definitions.getOrElse(
    id,
    parent.getOrElse(throw EllsDefinitionNotFound()).get(id)
  )

  def set(id: EllsIdentifier, value: EllsType): Unit =
    definitions.get(id) match {
      case Some(_) => definitions.update(id, value)
      case None => parent.getOrElse(throw EllsDefinitionNotFound()).set(id, value)
    }
}
