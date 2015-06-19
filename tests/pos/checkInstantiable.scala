// check instantiable of parameterized self type
class LS[T] { self: LS[T] => }
object Test {
  new LS[Int]
}
