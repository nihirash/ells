package com.nihirash.ells
import com.nihirash.ells.lib.SpecialForm
import org.scalatest.{FreeSpec, Matchers}

class CustomFormSpec extends FreeSpec with Matchers {
  import com.nihirash.ells.Implicits._

  class CustomForm(val eval: (EllsType, Env) => EllsType) extends SpecialForm {
    override def call(id: EllsIdentifier, args: List[EllsType], env: Env): Option[EllsType] = id.v match {
      case "test" => Some(1)
      case "hello" =>
        Some {
          "hello " + args.map(eval(_, env)).head.toString
        }
      case "some" =>
        Some {
          EllsList(1, 2, 3)
        }
      case _ => None
    }
  }

  class MyEval extends Eval {
    override val forms: Seq[SpecialForm] = baseForms :+ new CustomForm(eval)
  }

  val eval = new MyEval

  "Custom forms" - {
    "Will eval custom forms" in {
      Parser("(list (test) (hello \"world\") (some))").map(eval.eval(_, Env.preDef)) shouldEqual Right(
        EllsList(1, "hello world", EllsList(1, 2, 3))
      )
    }
  }
}
