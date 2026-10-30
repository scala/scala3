import language.experimental.captureChecking
import caps.*

package i26000subst:
  class File:
    def use(): Unit = ()
  class Box[A](val value: A)
  object io extends File, SharedCapability

  // The instance of `A` is the type of a closure, whose capture sets typer
  // does not know, so the parameter type of the lambda is inferred
  object Lib uses io:
    private def mk[A](a: A) = Box((f: List[A] ->{io} Unit) => f(List(a)))

    def test() =
      val b = mk(() => io.use())
      b.value(g => ())
