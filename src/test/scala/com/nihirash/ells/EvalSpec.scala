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

      toEval.map(eval.evalExpression(_, Env.empty)) shouldEqual toEval

    }

    "quote" - {
      "will return list forms unequaled" in {
        val toParse = "(quote (+ 1 2 3))"
        val parsed = Parser(toParse)
        parsed.map(x => eval.evalExpression(x.head, Env.empty)) shouldEqual Right(EllsList(List(EllsIdentifier("+"), EllsLong(1), EllsLong(2), EllsLong(3))))
      }

      "will fail on more than one arg" in {
        val toParse = "(quote 1 2 3)"
        val parsed = Parser(toParse)
        assertThrows[EllsArityException](parsed.map(value => eval.evalExpression(value.head, Env.empty)))
      }

      "will eval unquoted values" in {
        val expression1 = "'(1 2 @(+ 1 2))"
        val expression2 =
          s"""
             |(def var 'x)
             |'(def @var (list 1 2 3 @(+ 1 2 3)))
           """.stripMargin

        Parser(expression1).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsList(List(
          EllsLong(1), EllsLong(2), EllsLong(3)
        )))

        Parser(expression2).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsList(List(
          EllsIdentifier("def"),
          EllsIdentifier("x"),
          EllsList(List(
            EllsIdentifier("list"),
            EllsLong(1),
            EllsLong(2),
            EllsLong(3),
            EllsLong(6)
          ))
        )))
      }
    }

    "eval" - {
      "will eval quoted expression" in {
        val env = Env.empty
        val expression =
          """
            |(def x 1)
            |(defn set! (var val)
            | (eval '(set @var @val)))
            |
            | (set! 'x 2)
            | x
          """.stripMargin
        val evaled = Parser(expression).map(eval.eval(_, env))

        evaled shouldEqual Right(EllsLong(2))
      }
    }

    "operator +" - {
      "should sum all elements" in {
        val toParse = "(+ 1.0 2.2 (+ 1 2) -1)"
        val parsed = Parser(toParse)
        parsed.map(value => eval.evalExpression(value.head, Env.empty)) shouldEqual Right(EllsDouble(5.2))

      }
      "should fail on non numeric arguments" in {
        val toParse = "(+ 1 \"alex\")"
        val parsed = Parser(toParse)
        parsed match {
          case Right(value) => assertThrows[EllsTypesException](eval.evalExpression(value.head, Env.empty))
          case _ => fail("Parsing failure")
        }
      }
    }

    "operator -" - {
      "should subtract values" in {
        val toParse = "(- 5 (+ 1 2))"
        val parsed = Parser(toParse)
        parsed.map(eval.eval(_, Env.empty)) shouldEqual Right(EllsLong(2))
      }
    }

    "operator *" - {
      "should multiply values" in {
        val toParse = "(* 3 (+ 1 2) (- 11 1))"
        val parsed = Parser(toParse)
        parsed.map(eval.eval(_, Env.empty)) shouldEqual Right(EllsLong(90))
      }
    }

    "operator /" - {
      "should divide values" in {
        val toParse = "(/ 9.0 (+ 1 2) (- 11 1))"
        val parsed = Parser(toParse)
        parsed.map(eval.eval(_, Env.empty)) shouldEqual Right(EllsDouble(0.3))
      }

      "should fail on division on zero" in {
        val toParse = "(/ 9.0 0)"
        val parsed = Parser(toParse)
        parsed match {
          case Right(value) => assertThrows[ArithmeticException](eval.evalExpression(value.head, Env.empty))
          case _ => fail("Parsing failure")
        }
      }
    }

    "list" - {
      "will create list from arguments" in {
        val toParse = "(list \"hello, world\" (+ 2 2) (- 1 3))"
        val parsed = Parser(toParse)
        parsed.map(eval.eval(_, Env.empty)) shouldEqual Right(EllsList(List(
          EllsString("hello, world"),
          EllsLong(4),
          EllsLong(-2)
        )))
      }
    }

    "head" - {
      "will return first element of list" in {
        val toParse = "(head (list 1 2 3))"
        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsLong(1))
      }
    }

    "tail" - {
      "will return tail of list" in {
        val toParse = "(tail (quote (1 2 3)))"
        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsList(List(
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

        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsBoolean(true))
      }
    }

    "append" - {
      "will append elements to list" in {
        val toParse = "(list-append (quote (1 2 3)) 4 5)"
        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsList(List(
          EllsLong(1), EllsLong(2), EllsLong(3), EllsLong(4), EllsLong(5)
        )))
      }

      "will create list and append elements if first argument aren't list" in {
        val toParse = "(list-append 1 2 3 4 5)"
        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsList(List(
          EllsLong(1), EllsLong(2), EllsLong(3), EllsLong(4), EllsLong(5)
        )))
      }
    }

    "min" - {
      "will find minimal value of arguments" in {
        val toParse = "(min (list 4 2 3.3))"
        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsLong(2))
      }
    }

    "max" - {
      "will find maximal value of arguments" in {
        val toParse = "(max (list 0.75 (- 1 0.9) 3.3))"
        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsDouble(3.3))
      }
    }

    "operator >" - {
      "will return true is first argument more than second" in {
        val toParse = "(> 3 1)"
        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsBoolean(true))
      }

      "will return false is first argument less than second" in {
        val toParse = "(> 0 1)"
        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsBoolean(false))
      }
    }

    "operator <" - {
      "will return true is first argument less than second" in {
        val toParse = "(< 3 11.2)"
        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsBoolean(true))
      }

      "will return false is first argument more than second" in {
        val toParse = "(< 12.3 3)"
        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsBoolean(false))
      }
    }

    "operator =" - {
      "will return true if values are equal with numberic types" in {
        val toParse = "(= 1 1.0)"
        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsBoolean(true))
      }

      "will return true if values are equal with strings" in {
        val toParse = "(= \"hello\" \"hello\")"
        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsBoolean(true))
      }

      "will return false on different data types" in {
        val toParse = "(= 1 \"hello\")"
        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsBoolean(false))
      }

      "will return false on non equal values" in {
        val toParse = "(= 1 1.2)"
        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsBoolean(false))
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

        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsDouble(4))
      }

      "will return left-form if expression is nil" in {
        val toParse =
          """
            |(if nil
            |   (+ 2 2)
            |   (* 2 8))
          """.stripMargin

        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsLong(16))
      }

      "will return nil if left-form absent but expression is nil" in {
        val toParse =
          """
            |(if (> 2 16)
            |   (* 2 8))
          """.stripMargin

        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsNil())
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

        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsLong(8))
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

        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsLong(5))
      }
    }

    "def" - {
      "will define in current env definition" in {
        val toParse =
          """
            |(def x (* 2 5 6))
            |x
          """.stripMargin
        val env = Env.empty

        Parser(toParse).map(eval.eval(_, env)) shouldEqual Right(EllsDouble(60))
      }

      "will redefine old value in current when you call def twice but safes parent level definition" in {
        val toParse =
          """
            |(def x nil)
            |(def x (* 2 5 6))
            |x
          """.stripMargin
        val id = EllsIdentifier("x")
        val bTrue = EllsBoolean(true)
        val parent = Env(None, collection.mutable.Map(id -> bTrue))
        val env = Env(parent = Some(parent))

        Parser(toParse).map(eval.eval(_, env)) shouldEqual Right(EllsDouble(60))
        parent.definitions should contain theSameElementsAs Map(id -> bTrue)
      }
    }

    "set" - {
      "will redefine value searched lexicaly" in {
        val toParse =
          """
            |(def x (+ 5 5))
            |(set x 15)
            |(set y (* 2 8))
            |(list x y)
          """.stripMargin
        val parent = Env(None, collection.mutable.Map(EllsIdentifier("y") -> EllsDouble(5)))
        val env = Env(parent = Some(parent))

        Parser(toParse).map(eval.eval(_, env)) shouldEqual Right(EllsList(List(EllsDouble(15), EllsDouble(16))))
      }

      "will fail on absent definitions" in {
        val toParse = "(set x 100500)"
        assertThrows[EllsDefinitionNotFound](Parser(toParse).map(eval.eval(_, Env.empty)))
      }
    }

    "and" - {
      "will eval arg by arg until get nil" in {
        val toParse =
          """
            |(def x 1)
            |(def y 2)
            |(def z 3)
            |(and (> y x) (< y z) (= z 3))
          """.stripMargin

        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsBoolean(true))
      }
    }

    "or" - {
      "will stop evalation on first non nil value and return true" in {
        val toParse =
          """
            |(or
            | ()
            | (+ 1 1)
            | (/ 1 0))
          """.stripMargin

        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsBoolean(true))
      }
    }

    "not" - {
      "will inverse expressions boolean value" in {
        val toParse =
          """
            |(list (not true) (not ()) (not (not false)))
          """.stripMargin

        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsList(List(
          EllsBoolean(false), EllsBoolean(true), EllsBoolean(false)
        )))
      }
    }

    "fn" - {
      "will return function without params, when it called with nil as params args" in {
        val toParse =
          """
            |(fn () "hello, world")
          """.stripMargin
        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(
          EllsFunction(List.empty, Seq(EllsString("hello, world")))
        )
      }
    }

    "defn" - {
      "will defines function in local scope" in {
        val toParse =
          """
            |(defn inc (x)
            | (+ x 1))
            |
            | (inc 1)
          """.stripMargin

        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsLong(2))
      }
    }

    "is? functions" - {
      "is-nil? will return true when value is nil" in {
        Parser("(is-nil? ())").map(eval.eval(_, Env.empty)) shouldEqual Right(EllsBoolean(true))
      }

      "is-nil? will return false when value isn't nil" in {
        Parser("(is-nil? true)").map(eval.eval(_, Env.empty)) shouldEqual Right(EllsBoolean(false))
      }

      "is-number? will return true when argument is number" in {
        Parser("(is-number? (+ 1 2))").map(eval.eval(_, Env.empty)) shouldEqual Right(EllsBoolean(true))
      }

      "is-number? will return false when argument isn't number" in {
        Parser("(is-number? \"Hello\")").map(eval.eval(_, Env.empty)) shouldEqual Right(EllsBoolean(false))
      }

      "is-boolean? will return true when argument is boolean" in {
        Parser("(is-boolean? true)").map(eval.eval(_, Env.empty)) shouldEqual Right(EllsBoolean(true))
      }

      "is-boolean? will return false when argument isn't boolean" in {
        Parser("(is-boolean? nil)").map(eval.eval(_, Env.empty)) shouldEqual Right(EllsBoolean(false))
      }

      "is-fun? will return true when argument is function" in {
        Parser("(is-fun? (fn () \"ok\"))").map(eval.eval(_, Env.empty)) shouldEqual Right(EllsBoolean(true))
      }

      "is-fun? will return false when argument isn't function" in {
        Parser("(is-fun? nil)").map(eval.eval(_, Env.empty)) shouldEqual Right(EllsBoolean(false))
      }

      "is-list? will return true when argument is list" in {
        Parser("(is-list? '(hello))").map(eval.eval(_, Env.empty)) shouldEqual Right(EllsBoolean(true))
      }

      "is-list? will return true when argument is nil" in {
        Parser("(is-list? ())").map(eval.eval(_, Env.empty)) shouldEqual Right(EllsBoolean(true))
      }

      "is-list? will return false when argument isn't list" in {
        Parser("(is-list? 123)").map(eval.eval(_, Env.empty)) shouldEqual Right(EllsBoolean(false))
      }
    }

    "to-string" - {
      "will convert any type to string" in {
        Parser("(to-string '(1 2 @(+ 1 2)))").map(eval.eval(_, Env.empty)) shouldEqual Right(EllsString("(1 2 3.0)"))
      }
    }

    "to-number" - {
      "will try convert string to number" in {
        Parser("(to-number \"123\")").map(eval.eval(_, Env.empty)) shouldEqual Right(EllsDouble(123))
      }

      "will crash when converting imposible" in {
        assertThrows[NumberFormatException](Parser("(to-number \"isnt number\")").map(eval.eval(_, Env.empty)))
      }
    }

    "try/throw" - {
      "throw will throw exception" in {
        assertThrows[EllsRuntimeException](Parser("(throw \"Bye bye\")").map(eval.eval(_, Env.empty)))
      }

      "try will execute catch block on exception" in {
        val code =
          """
            |(try
            | (do
            |   (throw "My exception")
            |   1)
            | (= exception "My exception"))
          """.stripMargin

        Parser(code).map(eval.eval(_)) shouldEqual Right(EllsBoolean(true))
      }
    }

    "named and anonymous functions" - {
      "will eval it" in {
        val toParse =
          """
            |(defn map (fun lst)
            |    "This function applies function to every element of list and returns resulting list"
            |    (defn in-map (lst acc)
            |        (def h (head lst))
            |        (if (tail lst)
            |            (in-map (tail lst) (list-append acc (fun h)))
            |            (list-append acc (fun h))))
            |
            |        (in-map lst ()))
            |
            |(map (fn (x) (+ x 1)) (list 0 1 2 3))
          """.stripMargin

        Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsList(List(
          EllsLong(1), EllsLong(2), EllsLong(3), EllsLong(4)
        )))
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
      Parser(toParse).map(eval.eval(_, Env.empty)) shouldEqual Right(EllsString("But returns this string"))
    }
  }
}