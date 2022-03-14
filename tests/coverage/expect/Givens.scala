package covtest

import scala.language.strictEquality

class Context(val id: Int)

object Givens:

  def test(): Unit =
    given CanEqual[Int, String] = CanEqual.derived
    println(3 == "3")
    println(3 == 5.1)

  def printContext(msg: String)(using ctx: Context): Unit =
    println(msg)
    println(ctx.id)

  def getMessage(i: Int): String = i.toString

  def test2() =
    // TODO FIXME: this does not work if we remove the def declaration (compiler crash in LambdaLift)
    given c: Context = Context(0)
    printContext("test")(using c)
    printContext(getMessage(123))
