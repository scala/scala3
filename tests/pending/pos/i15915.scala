class _Monoid[A]
object _Monoid {
  implicit val Monoid: _Monoid[Int] = new _Monoid[Int]
}

class Lifecycle[A]
object Lifecycle {

  implicit def monoidForLifecycle[Monoid[_], A](
    implicit
    monoidType: GetMonoidType[Monoid],
    monoidA: Monoid[A]
  ): Monoid[Lifecycle[A]] = new _Monoid().asInstanceOf[Monoid[Lifecycle[A]]]

}

sealed class GetMonoidType[C[_]]
object GetMonoidType {
  implicit val getMonoid: GetMonoidType[_Monoid] = new GetMonoidType[_Monoid]
}

object App extends App {
  println(implicitly[_Monoid[Lifecycle[Int]]])
}