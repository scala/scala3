import scala.annotation.unchecked.uncheckedVariance as uV

trait Test:
  def test[S] =
    val a: (([O] =>> Foo[O, S]) @uV)[Int] = ???
    a.m()

class Foo[X, Y]:
  def m(): Y = ???
