import scala.language.dynamics

class Foo extends Dynamic {
  def applyDynamic(name: String)(): Bar = ???
}

class Bar extends Dynamic {
  def applyDynamic(name: String)(x: Int) = ???
}

val foo = new Foo
def baz = foo.blah().apply(42)
