package com.nihirash.ells

import org.scalatest._

class EvalSpec extends FreeSpec with Matchers {
  val eval = new Eval

  "evalExpression" - {
    "should return scalar types as is" in {
      val doubleVal = EllsDouble(2.3)
      val longVal = EllsDouble(3)
      val stringVal = EllsString("test string")
      val nilVal = EllsNil()

      val toEval = Seq(doubleVal, longVal, stringVal, nilVal)

      toEval.map(eval.evalExpression) shouldEqual toEval

    }

    "quote" - {
      "will return list forms unequaled" in {
        val toParse = "(quote (+ 1 2 3))"
        val parsed = Parser(toParse)
        parsed.map(x => eval.evalExpression(x.head)) shouldEqual Right(EllsList(List(EllsIdentifier("+"), EllsLong(1), EllsLong(2), EllsLong(3))))
      }

      "will fail on more than one arg" in {
        val toParse = "(quote 1 2 3)"
        val parsed = Parser(toParse)
        assertThrows[EllsArityException](parsed.map(value => eval.evalExpression(value.head)))
      }
    }

    "operator +" - {
      "should sum all elements" in {
        val toParse = "(+ 1.0 2.2 (+ 1 2) -1)"
        val parsed = Parser(toParse)
        parsed.map(value => eval.evalExpression(value.head)) shouldEqual Right(EllsDouble(5.2))

      }
      "should fail on non numeric arguments" in {
        val toParse = "(+ 1 \"alex\")"
        val parsed = Parser(toParse)
        parsed match {
          case Right(value) => assertThrows[EllsTypesException](eval.evalExpression(value.head))
          case _ => fail("Parsing failure")
        }
      }
    }

    "operator -" - {
      "should subtract values" in {
        val toParse = "(- 5 (+ 1 2))"
        val parsed = Parser(toParse)
        parsed.map(eval.eval) shouldEqual Right(EllsLong(2))
      }
    }

    "operator *" - {
      "should multiply values" in {
        val toParse = "(* 3 (+ 1 2) (- 11 1))"
        val parsed = Parser(toParse)
        parsed.map(eval.eval) shouldEqual Right(EllsLong(90))
      }
    }

    "operator /" - {
      "should divide values" in {
        val toParse = "(/ 9.0 (+ 1 2) (- 11 1))"
        val parsed = Parser(toParse)
        parsed.map(eval.eval) shouldEqual Right(EllsDouble(0.3))
      }

      "should fail on division on zero" in {
        val toParse = "(/ 9.0 0)"
        val parsed = Parser(toParse)
        parsed match {
          case Right(value) => assertThrows[ArithmeticException](eval.evalExpression(value.head))
          case _ => fail("Parsing failure")
        }
      }
    }
  }

  "eval" - {
    "should return last value" in {
      val toParse =
        """
          |
          |"This is small test"
          |
          |(+ 1 2 3
          | (* 1 2 3)
          | (+ 1 (+ 2 3 4)))
          |"But returns this string"
        """.stripMargin
      Parser(toParse).map(eval.eval) shouldEqual Right(EllsString("But returns this string"))
    }
  }
}
