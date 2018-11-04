package com.nihirash.ells

import com.nihirash.ells.lib._

import scala.collection.mutable.{Map => MutableMap}

class Eval {
  final val baseForms = Seq(
    new BaseForms(eval),
    new ListForms(eval),
    new MathForms(eval),
    new StringForms(eval)
  )

  val forms: Seq[SpecialForm] = baseForms

  def eval(exprs: Seq[EllsType], env: Env = Env.empty): EllsType = exprs.map(eval(_, env)).last

  def eval(e: EllsType, env: Env): EllsType = e match {
    case e: EllsScalar     => e
    case l: EllsList       => evalForm(l, env)
    case i: EllsIdentifier => env.get(i)
    case _                 => throw new Exception(s"Can't eval $e")
  }

  private def evalForm(l: EllsList, env: Env): EllsType = {
    val car = l.v.head
    val cdr = l.v.tail
    car match {
      case i: EllsIdentifier => evalCall(i, cdr, env)
      case l: EllsFunction   => evalFunction(l, cdr, env)
      case f                 => throw new RuntimeException(s"Cant eval form: $l")
    }
  }

  private def evalFunction(fn: EllsFunction, args: List[EllsType], env: Env): EllsType = {
    if (fn.args.length != args.length) throw EllsArityException()
    val argValues = fn.args zip args
    val innerEnv = Env(
      parent = Some(env),
      definitions = MutableMap(argValues: _*)
    )
    eval(fn.body, innerEnv)
  }

  private def evalCall(id: EllsIdentifier, args: List[EllsType], env: Env): EllsType = {

    def tryEval(forms: Seq[SpecialForm], id: EllsIdentifier, args: List[EllsType], env: Env): Option[EllsType] =
      forms match {
        case Nil => None
        case form :: _ =>
          form.call(id, args, env) match {
            case None                   => tryEval(forms.tail, id, args, env)
            case result: Some[EllsType] => result
          }
      }

    tryEval(forms, id, args, env).getOrElse(env.get(id) match {
      case EllsNil()       => EllsNil()
      case f: EllsFunction => evalFunction(f, args.map(eval(_, env)), env)
      case _               => throw EllsEvalException(s"Can't eval form '$id' with args '$args'")
    })
  }
}
