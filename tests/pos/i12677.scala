class F[A]
object F {
  def apply[A](a: => A) = new F[A]
}

trait TC[A] { type Out }
object TC {
  implicit def tc[A]: TC[A] { type Out = String } = ???
}

// ====================================================================================
object Bug {
  final class CustomHook[A] {
    def blah(implicit tc: TC[A]): CustomHook[tc.Out] = ???
  }

  def i: CustomHook[Int] = ???
  val f = F(i.blah)
  f: F[CustomHook[String]] // error
}

// ====================================================================================
object Workaround {
  final class CustomHook[A] {
    def blah[B](implicit tc: TC[A] { type Out = B }): CustomHook[B] = ??? // raise type
  }

  def i: CustomHook[Int] = ???
  val f = F(i.blah)
  f: F[CustomHook[String]] // works
}