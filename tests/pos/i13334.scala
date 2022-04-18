class Bar

transparent inline def bar(i: Int): Bar =
  new Bar:
    def j = i

class Foo(x: Int):
  def foo = bar(x)

trait DFC
given DFC = new DFC {}

trait TC
object TC:
  def foo()(using DFC): Unit = {}

  transparent inline given (using DFC): TC = new TC:
    foo()

class Baz(using DFC):
  summon[TC]

val top = new Baz
