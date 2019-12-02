import scala.compiletime
trait Foo[A]


trait Qux {
  given Foo[Int] = new Foo[Int] {}
}