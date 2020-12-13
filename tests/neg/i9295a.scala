import scala.language.dynamics

class Foo extends Dynamic {
  def applyDynamic(arg: Any): Foo = ???
}
object F {
  val foo = new Foo
  def baz = foo.blah(43) // error: method applyDynamic in class Foo does not take more parameters
}
