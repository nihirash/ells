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

    "list" - {
      "will create list from arguments" in {
        val toParse = "(list \"hello, world\" (+ 2 2) (- 1 3))"
        val parsed = Parser(toParse)
        parsed.map(eval.eval) shouldEqual Right(EllsList(List(
          EllsString("hello, world"),
          EllsLong(4),
          EllsLong(-2)
        )))
      }
    }

    "head" - {
      "will return first element of list" in {
        val toParse = "(head (list 1 2 3))"
        Parser(toParse).map(eval.eval) shouldEqual Right(EllsLong(1))
      }
    }

    "tail" - {
      "will return tail of list" in {
        val toParse = "(tail (quote (1 2 3)))"
        Parser(toParse).map(eval.eval) shouldEqual Right(EllsList(List(
          EllsLong(2), EllsLong(3)
        )))
      }

      "will return nil if list contains one element" in {
        val toParse =
          """
            |(if (tail (list 1))
            |   false
            |   true)
          """.stripMargin

        Parser(toParse).map(eval.eval) shouldEqual Right(EllsBoolean(true))
      }
    }

    "min" - {
      "will find minimal value of arguments" in {
        val toParse = "(min (list 4 2 3.3))"
        Parser(toParse).map(eval.eval) shouldEqual Right(EllsLong(2))
      }
    }

    "max" - {
      "will find maximal value of arguments" in {
        val toParse = "(max (list 0.75 (- 1 0.9) 3.3))"
        Parser(toParse).map(eval.eval) shouldEqual Right(EllsDouble(3.3))
      }
    }

    "operator >" - {
      "will return true is first argument more than second" in {
        val toParse = "(> 3 1)"
        Parser(toParse).map(eval.eval) shouldEqual Right(EllsBoolean(true))
      }

      "will return false is first argument less than second" in {
        val toParse = "(> 0 1)"
        Parser(toParse).map(eval.eval) shouldEqual Right(EllsBoolean(false))
      }
    }

    "operator <" - {
      "will return true is first argument less than second" in {
        val toParse = "(< 3 11.2)"
        Parser(toParse).map(eval.eval) shouldEqual Right(EllsBoolean(true))
      }

      "will return false is first argument more than second" in {
        val toParse = "(< 12.3 3)"
        Parser(toParse).map(eval.eval) shouldEqual Right(EllsBoolean(false))
      }
    }

    "operator =" - {
      "will return true if values are equal with numberic types" in {
        val toParse = "(= 1 1.0)"
        Parser(toParse).map(eval.eval) shouldEqual Right(EllsBoolean(true))
      }

      "will return true if values are equal with strings" in {
        val toParse = "(= \"hello\" \"hello\")"
        Parser(toParse).map(eval.eval) shouldEqual Right(EllsBoolean(true))
      }

      "will return false on different data types" in {
        val toParse = "(= 1 \"hello\")"
        Parser(toParse).map(eval.eval) shouldEqual Right(EllsBoolean(false))
      }

      "will return false on non equal values" in {
        val toParse = "(= 1 1.2)"
        Parser(toParse).map(eval.eval) shouldEqual Right(EllsBoolean(false))
      }
    }

    "if-form" - {
      "will return right-form if expression isn't nil" in {
        val toParse =
          """
            |(if (= 1 (- 2 1))
            |   (+ 2 2)
            |   (- 2 2))
          """.stripMargin

        Parser(toParse).map(eval.eval) shouldEqual Right(EllsDouble(4))
      }

      "will return left-form if expression is nil" in {
        val toParse =
          """
            |(if nil
            |   (+ 2 2)
            |   (* 2 8))
          """.stripMargin

        Parser(toParse).map(eval.eval) shouldEqual Right(EllsLong(16))
      }

      "will return nil if left-form absent but expression is nil" in {
        val toParse =
          """
            |(if (> 2 16)
            |   (* 2 8))
          """.stripMargin

        Parser(toParse).map(eval.eval) shouldEqual Right(EllsNil())
      }

      "will return right form when left-form absent and expression isn't nil" in {
        val toParse =
          """
            |(if (< 2 16)
            |   (do
            |     "I'm fine!"
            |     (* 3 3)
            |     (+ 4 4)
            |   ))
          """.stripMargin

        Parser(toParse).map(eval.eval) shouldEqual Right(EllsLong(8))
      }
    }
  }

  "do-form" - {
    "will eval code block where waited one expression" in {
      val toParse =
        """
          |(if (do
          |       "There some strange code block"
          |       (+ 2 2))
          |     (+ 2 3))
        """.stripMargin

      Parser(toParse).map(eval.eval) shouldEqual Right(EllsLong(5))
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
