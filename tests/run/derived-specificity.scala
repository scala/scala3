import scala.deriving._

object Test extends App {
  case class Foo() derives Bar, Baz
  case class Bar[T](id: String)
  object Bar {
    implicit def barGen[T](implicit m: Mirror.Of[T]): Bar[T] = Bar("Bar.barGen")
    def derived[T](implicit m: Mirror.Of[T]): Bar[T] = Bar("Bar.derived")
  }

  trait Baz[T] {
    def id: String
  }
  object Baz {
    def apply[T](id0: String): Baz[T] = new Baz[T] {
      def id = id0
    }
    implicit def bazGen[T](implicit m: Mirror.Of[T]): Baz[T] = Baz("Baz.bazGen")
    def derived[T](implicit m: Mirror.Of[T]): Baz[T] = Baz("Baz.derived")
  }

  val bar = implicitly[Bar[Foo]]
  assert(bar.id == "Bar.derived") 

  val baz = implicitly[Baz[Foo]]
  assert(baz.id == "Baz.derived") 
}

