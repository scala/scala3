trait T[X]

trait U[X]

trait TC[M[_]] {
  def foo[M[_]: TC, A](ma: U[A]) = ()
  implicit val TCofT: TC[T] = new TC[T] {}
  implicit def any2T[A](a: A): T[A] = new T[A] {}
  implicit def any2U[A](a: A): U[A] = new U[A] {}
  val x = foo[T, Int](1)
  val y = ()
}

// Minimized version exhibiting an assertion violation in Denotation#current at phase lambdalift:
trait TC2 {
//  implicit val TCofT: TC2[T] = new TC2[T] {}
  val TCofT: Object = {
    class C extends TC2
    new Object
  }
}
