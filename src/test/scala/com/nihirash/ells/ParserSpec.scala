package com.nihirash.ells

import fastparse.core.Parsed
import org.scalatest._

class ParserSpec extends FreeSpec with Matchers {
  "Numbers parser" - {
    "should parse long numbers correctly" in {
      val valueToParse = "123"
      val expected: Long = 123L

      Parser.numberParser.parse(valueToParse) match {
        case Parsed.Success(EllsLong(value), index) => value shouldEqual expected
        case _ => fail("Number doesn't parsed")
      }
    }

    "should parse double numbers correctly" in {
      val valueToParse = "12.3E-1"
      val expected: Double = 1.23

      Parser.numberParser.parse(valueToParse) match {
        case Parsed.Success(EllsDouble(v), index) => v shouldEqual expected
        case _ => fail("Number doesn't parsed")
      }
    }

    "should fails on non-numbers" in {
      val valueToParse = "@ww22"

      Parser.numberParser.parse(valueToParse) match {
        case Parsed.Failure(_, _, _) => succeed
        case _ => fail("Wrong value parsed")
      }
    }
  }

  "Strings parser" - {
    "should extract any string from quotas" in {
      val valueToParse =
        """
          | "Hello, world! Проверка различных символов! 💣"
        """.stripMargin

      val expected = "Hello, world! Проверка различных символов! 💣"

      Parser.stringParser.parse(valueToParse) match {
        case Parsed.Success(EllsString(v), index) => v shouldEqual expected
        case _ => fail("String parsed wrong")
      }
    }

    "should extract escaped values" in {
      val valueToParse =
        """
          | "Ну дратути,\tнаш \"друг\"\r\n"
        """.stripMargin

      val expected = "Ну дратути,\tнаш \"друг\"\r\n"

      Parser.stringParser.parse(valueToParse) match {
        case Parsed.Success(EllsString(v), index) => v shouldEqual expected
        case _ => fail("Escaped string parse failure")
      }
    }

    "should fails when no paired quotas" in {
      val valueToParse =
        """
          | "Hello, world
        """.stripMargin

      Parser.stringParser.parse(valueToParse) match {
        case Parsed.Failure(_, _, _) => succeed
        case _ => fail("Succesfully parsed non paired quotas")
      }
    }
  }

  "Identity parser" - {
    "should extract identity properly" in {
      val valueToParse = "some-identity_v2!"

      Parser.identityParser.parse(valueToParse) match {
        case Parsed.Success(EllsIdentifier(id), _) => id shouldEqual valueToParse
        case _ => fail("Identifier doesn't parsed")
      }
    }

    "should fail if identifier begins from number" in {
      val valueToParse = "111GalaxyInDanger"

      Parser.identityParser.parse(valueToParse) match {
        case Parsed.Success(_, _) => fail("Wrong identifier was parsed")
        case Parsed.Failure(_, _, _) => succeed
      }
    }

    "should parse identifiers that's contains only special characters" in {
      val valueToParse = ">="

      Parser.identityParser.parse(valueToParse) match {
        case Parsed.Success(EllsIdentifier(id), _) => id shouldEqual valueToParse
        case _ => fail("Identifier doesn't parsed")
      }
    }
  }

  "Boolean parser" - {
    "should parse true value" in {
      val valueToParse = "true"

      Parser.booleanParser.parse(valueToParse) match {
        case Parsed.Success(EllsBoolean(b), _) => b shouldEqual true
        case _ => fail("true boolean value doesn't parsed")
      }
    }

    "should parse false value" in {
      val valueToParse = "false"

      Parser.booleanParser.parse(valueToParse) match {
        case Parsed.Success(EllsBoolean(b), _) => b shouldEqual false
        case _ => fail("false boolean value doesn't parsed")
      }
    }

    "should fails on wrong values" in {
      val valueToParse = "Flalse"

      Parser.booleanParser.parse(valueToParse) match {
        case Parsed.Success(_, _) => fail("Wrong boolean was parsed")
        case Parsed.Failure(_, _, _) => succeed
      }
    }
  }

  "nil parser" - {
    "should extract nil value" in {
      val valueToParse = "nil"

      Parser.nilParser.parse(valueToParse) match {
        case Parsed.Success(EllsNil(), _) => succeed
        case _ => fail("Nil doesn't parsed")
      }
    }
  }

  "list parser" - {
    "should parse list of EllsTypes" in {
      val valueToParse = "(1 2 3)"

      Parser.listParser.parse(valueToParse) match {
        case Parsed.Success(EllsList(v), _) => v shouldEqual List(1L, 2L, 3L).map(EllsLong)
        case _ => fail("List parsing failed")
      }
    }

    "should parse recursive lists" in {
      val valueToParse =
        """(hello
          | (world 1
          |        12.3
          |        "Привет!"
          |        ()))""".stripMargin

      val expected = EllsList(List(
        EllsIdentifier("hello"),
        EllsList(List(
          EllsIdentifier("world"),
          EllsLong(1),
          EllsDouble(12.3),
          EllsString("Привет!"),
          EllsNil()
        ))
      ))

      Parser.listParser.parse(valueToParse) match {
        case Parsed.Success(result, _) => result shouldEqual expected
        case _ => fail("Recursive parsing list failed")
      }
    }

    "should fail on unpaired brackets" in {
      val valueToParse = "(hello world"

      Parser.listParser.parse(valueToParse) match {
        case Parsed.Success(_, _) => fail("Worng list parsed")
        case Parsed.Failure(_, _, _) => succeed
      }
    }
  }
}
