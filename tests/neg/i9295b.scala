import scala.language.dynamics

class Foo extends Dynamic {
  def applyDynamic(arg: Any): Bar = ???
}
class Bar extends Dynamic {
  def applyDynamic(arg: Any)(x: Int): Int = ???
}
object F {
  val foo = new Foo
  def baz = foo.blah(43) // error: method applyDynamic in class Foo does not take more parameters
  def qux = foo.blah.blah(43) // error: value selectDynamic is not a member of Foo
  def quxx = foo.blah().blah(43) // error: method applyDynamic in class Foo does not take more parameters
}
