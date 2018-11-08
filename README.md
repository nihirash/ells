[![Build Status](https://travis-ci.org/nihirash/ells.svg?branch=master)](https://travis-ci.org/nihirash/ells) 
[![Latest version](https://maven-badges.herokuapp.com/maven-central/io.github.nihirash/ells_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.github.nihirash/ells_2.12)

# ELLS
Embeddable Lisp-Like Scripting project.

## Installation

Just add artifact to your dependencies: `"io.github.nihirash" %% "ells" % <latest version>`

## Usage

### Quick start guide

Simplest way to begin is this part of code:

```
import com.nihirash.ells.{Env, Eval, Interpreter}

val eval = new Eval()
val env = Env.preDef
val interpreter = new Interpreter(eval, env)

interpreter.run(
    """
      |(defn hello (name)
      | (string-append "hello " name))
      |
      | (hello "world")
    """.stripMargin).map(r => println(r.result))
```

And You'll get `hello world` on your console.

### Predefined forms and default evaluator

If You'll use default evaluator there will be something like "standard library" for ELLS.

You may define yourself evaluator with your custom forms, and/or use some of default forms of ELLS.

Information about standard forms present in `docs/forms` folder.

Default predefined environment made by evaluating `src/main/resources/stdlib.ells` file. Please look into it for more information about predefined environment.

### Our custom special forms and evaluator

To make your special forms/bindings to scala you must create class with extending `com.nihirash.ells.lib.SpecialForm` trait.

Simplest example of custom special forms:

```
  class CustomForm(val eval: (EllsType, Env) => EllsType) extends SpecialForm {
    import com.nihirash.ells.Implicits._

    override def call(id: EllsIdentifier, args: List[EllsType], env: Env): Option[EllsType] = id.v match {
      case "print" => Some {
        val text = args.map(eval(_, env)).mkString("").toString
        println(text)
        text
      }
      case _ => None
    }
  }
```

Now we need evaluator that's support our special form. For this we need only append our new special form to forms sequence in new evaluator class:

```
class MyEval extends Eval {
  override val forms: Seq[SpecialForm] = baseForms :+ new CustomForm(eval)
}
```

And there now we can use it:

```
object Example extends App {

  val eval = new MyEval
  val env = Env.preDef
  val interpreter = new Interpreter(eval, env)

  interpreter.run(
    """
      |(defn hello (arg)
      | (string-append "hello, " arg "!"))
      |
      |(defn say-hello! (arg)
      | (print (hello arg)))
      |
      |(say-hello! "world")
    """.stripMargin)
}

```

## Sample app

Small example of embedding ELLS is present [here](https://github.com/nihirash/ells-example)

## Contributing

If you want to contribute to a project and make it better, your help is very welcome. Any contributions are welcome - misspell fixes, code or documentation improvements and any other.

## Legal

Copyright (c) 2018 Alexander Sharihin. All rights reserved.

