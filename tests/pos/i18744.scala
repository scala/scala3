package dotty.tools.dotc.typer

object Color:
  def apply(): Int = ???

extension (u: Unit)
  def foo(that: String, f: Int => Int): Int = ???
  def foo(that: Long, f: Int => Int): Int = ???

def test =
  val c = Color()
  ().foo("", (_: Int) => c)
  ().foo("", (_: Int) => Color())