package com.nihirash.ells
import com.nihirash.ells

case class EllsResult(result: EllsType, env: Env)

class Interpreter(evalator: Eval, initialEnv: Env) {
  def run(script: String, env: Env = initialEnv.copy()): Either[String, EllsResult] = {
    Parser(script).flatMap { ast =>
      try {
        val result = evalator.eval(ast, env)
        Right(ells.EllsResult(result, env))
      } catch {
        case x: Throwable =>
          Left(x.getLocalizedMessage)
      }
    }
  }
}
