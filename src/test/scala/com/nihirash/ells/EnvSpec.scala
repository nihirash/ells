package com.nihirash.ells

import org.scalatest._

class EnvSpec extends FreeSpec with Matchers {
  "Enviroment" - {
    "will set definition on define" in {
      val env = Env.empty
      env.define(id1, bTrue)

      env.definitions should contain theSameElementsAs Map(id1 -> bTrue)
    }

    "will update definition value on set" in {
      val env = Env.empty
      env.define(id1, bTrue)
      env.set(id1, bFalse)

      env.definitions should contain theSameElementsAs Map(id1 -> bFalse)
    }

    "will fail on set if value wasn't predefined" in {
      val env = Env.empty
      assertThrows[EllsDefinitionNotFound](env.set(id1, bTrue))
    }

    "will return value on get if it was defined" in {
      val env = Env.empty
      env.define(id1, bTrue)
      env.get(id1) shouldEqual bTrue
    }

    "will search definitions recursive" in {
      val parent = Env.empty
      val child = Env(parent = Some(parent))

      parent.define(id1, bTrue)

      child.get(id1) shouldEqual bTrue
    }

    "will update parents definition if it exists" in {
      val parent = Env.empty
      val child = Env(parent = Some(parent))

      parent.define(id1, bTrue)
      child.set(id1, bFalse)

      parent.definitions should contain theSameElementsAs Map(id1 -> bFalse)
    }

    "should make new definitions in child environment when same named definition are present in parent" in {
      val parent = Env.empty
      val child = Env(parent = Some(parent))
      parent.define(id1, bTrue)
      child.define(id1, bFalse)
      child.get(id1) shouldEqual bFalse
      parent.get(id1) shouldEqual bTrue
    }
  }

  "PreDef" - {
    "will provide predefined map and filter" in {
      val eval = new Eval
      val env = Env.preDef
      Parser("""
          |(def x '(1 2 3 4 5))
          |
          |(def res1 (map (fn (x) (+ 1 x)) x))
          |(def res2 (filter (fn (x) (> x 3)) x))
          |(list res1 res2 (n-th x 3))
        """.stripMargin)
        .map(eval.eval(_, env)) shouldEqual Right(
        EllsList(
          List(
            EllsList(
              List(
                EllsLong(2),
                EllsLong(3),
                EllsLong(4),
                EllsLong(5),
                EllsLong(6)
              )),
            EllsList(
              List(
                EllsLong(4),
                EllsLong(5)
              )),
            EllsLong(3)
          )))
    }
  }

  val id1 = EllsIdentifier("id1")
  val bTrue = EllsBoolean(true)
  val bFalse = EllsBoolean(false)
}
