package com.nihirash.ells

import org.scalatest._

class EnvSpec extends FreeSpec with Matchers {
  "Enviroment" - {
    "will set definition on define" in {
      val env = Env()
      env.define(id1, bTrue)

      env.definitions should contain theSameElementsAs Map(id1 -> bTrue)
    }

    "will update definition value on set" in {
      val env = Env()
      env.define(id1, bTrue)
      env.set(id1, bFalse)

      env.definitions should contain theSameElementsAs Map(id1 -> bFalse)
    }

    "will fail on set if value wasn't predefined" in {
      val env = Env()
      assertThrows[EllsDefinitionNotFound](env.set(id1, bTrue))
    }

    "will return value on get if it was defined" in {
      val env = Env()
      env.define(id1, bTrue)
      env.get(id1) shouldEqual bTrue
    }

    "will search definitions recursive" in {
      val parent = Env()
      val child = Env(parent = Some(parent))

      parent.define(id1, bTrue)

      child.get(id1) shouldEqual bTrue
    }

    "will update parents definition if it exists" in {
      val parent = Env()
      val child = Env(parent = Some(parent))

      parent.define(id1, bTrue)
      child.set(id1, bFalse)

      parent.definitions should contain theSameElementsAs Map(id1 -> bFalse)
    }

    "should make new definitions in child environment when same named definition are present in parent" in {
      val parent = Env()
      val child = Env(parent = Some(parent))
      parent.define(id1, bTrue)
      child.define(id1, bFalse)
      child.get(id1) shouldEqual bFalse
      parent.get(id1) shouldEqual bTrue
    }
  }

  val id1 = EllsIdentifier("id1")
  val bTrue = EllsBoolean(true)
  val bFalse = EllsBoolean(false)
}
