// https://github.com/scala/scala3/issues/13334
trait DFC
given DFC = new DFC {}

trait TC
object TC:
  def foo()(using DFC): Unit = {}

  inline given (using DFC): TC = new TC:
    foo()

class Foo(using DFC):
  summon[TC]

@main def Test() =
  val top = new Foo
