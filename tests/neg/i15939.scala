import scala.language.implicitConversions

object Test {
  class Foo {
    class Bar {
      override def toString() = "bar"
    }
    object Bar {
      implicit def fromString(a: String): Bar = { println("convert bar") ; new Bar }
    }

    def andThen(b: Bar): Unit = { println("pre") ; println(s"use $b") ; println("post") }
    def andThen_:(b: Bar) = { println("pre") ; println(s"use $b") ; println("post") }
    def andThenByName_:(b: => Bar) = { println("pre") ; println(s"use $b") ; println(s"use $b") ; println("post") }
  }

  def mkFoo: Foo = ???
  def mkBarString: String = ???
  mkFoo.andThen(mkBarString)  // error
  mkBarString andThen_: mkFoo  // error
  mkFoo.andThen_:(mkBarString)  // error
  mkBarString andThenByName_: mkFoo // error
  mkFoo.andThenByName_:(mkBarString) // error
}