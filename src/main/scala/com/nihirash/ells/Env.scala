package com.nihirash.ells

import scala.collection.mutable.{Map => MutableMap}
import scala.io.Source

case class Env(parent: Option[Env], definitions: MutableMap[EllsIdentifier, EllsType] = MutableMap.empty) {
  def define(id: EllsIdentifier, value: EllsType): Unit = definitions.update(id, value)

  def get(id: EllsIdentifier): EllsType = definitions.getOrElse(
    id,
    parent.getOrElse(throw EllsDefinitionNotFound(s"$id is undefined")).get(id)
  )

  def set(id: EllsIdentifier, value: EllsType): Unit =
    definitions.get(id) match {
      case Some(_) => definitions.update(id, value)
      case None    => parent.getOrElse(throw EllsDefinitionNotFound()).set(id, value)
    }
}

object Env {
  def empty: Env = Env(None, MutableMap.empty)

  lazy val preDefinedEnviroment: Env = {
    val code = Source.fromResource("stdlb.ells").getLines.mkString("\n")
    val env = empty
    val eval = new Eval
    Parser(code).map(eval.eval(_, env)) match {
      case Left(value) => throw EllsInternalError(value)
      case Right(_)    => env
    }
  }

  lazy val preDef: Env = preDefinedEnviroment.copy()
}
