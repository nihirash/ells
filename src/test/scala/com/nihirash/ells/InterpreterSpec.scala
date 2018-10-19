package com.nihirash.ells
import org.scalatest.{FreeSpec, Matchers}

class InterpreterSpec extends FreeSpec with Matchers {
  import Implicits._

  val ells = new Interpreter(new Eval, Env.preDef)

  "Ells interpreter" - {
    "Will interpret correct code and return changed environment and result of operations" in {
      val script =
        """
          |(def csv "1,2,3\n2,3,4\n3,4,5")
          |(def csv-parsed ())
          |
          |(defn parseCsv (csv)
          |  (def rows (split-string csv "\n"))
          |  (map
          |   (fn (row)
          |       (map
          |	(fn (element) (to-number element))
          |	(split-string row ",")))
          |   rows))
          |
          |
          |(set csv-parsed (parseCsv csv))
          |
          |(map (fn (row) (max row))
          |     csv-parsed)
          |
        """.stripMargin

      val result = ells.run(script)

      result match {
        case Right(EllsResult(EllsList(List(EllsDouble(3), EllsDouble(4), EllsDouble(5))), env))
            if env.get(EllsIdentifier("csv-parsed")) == EllsList(
              EllsList(1, 2, 3),
              EllsList(2, 3, 4),
              EllsList(3, 4, 5)
            ) =>
          succeed

        case _ => fail("Code doesn't eval properly")
      }
    }

    "Will return error message if parsing won't be success" in {
      val script = "(oh wait ..."

      ells.run(script).left.get.substring(0, 12) shouldEqual "Parse failed"

    }

    "Will return error message on runtime errors" in {
      val script = "(/ 1 0)"
      ells.run(script) shouldEqual Left("Division by zero")
    }
  }
}
