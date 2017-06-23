import reflect.ClassTag

trait Foo[T: ClassTag]() {
  def foo(x: T) = Array(x)
}
class Bar extends Foo[Int]()
object Test extends App {
  (new Bar).foo(3)
}