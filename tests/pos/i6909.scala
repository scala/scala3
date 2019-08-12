import scala.compiletime
trait Foo[A]


trait Qux {
  given as Foo[Int] = new Foo[Int] {}
}