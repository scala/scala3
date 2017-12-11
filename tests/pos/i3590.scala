trait Tagged[T]

object Tagged {
  type Aux[T, UNUSED] = Tagged[T]
}

trait Fun[R] {
  type Out
}

object Fun extends Fun0 {
  // In Dotty there is a difference between asking for Tagged.Aux[T, Int]
  // and asking for Tagged[T]. In the former case the companion of T is
  // not considered as a valid scope during implicit search. In scalac
  // both cases are treated equally.
  implicit def tagged[T](implicit t: Tagged.Aux[T, Int]): Fun[T] { type Out = Int } = ???
}

trait Fun0 {
  implicit def default[T]: Fun[T] { type Out = String } = ???
}

object FunDemo extends App {
  case class A(x: Int, y: String)
  object A {
    implicit val tag: Tagged[A] = ???
  }

  // Precise version of implicitly that keeps type members
  def the[T <: AnyRef](implicit ev: T): ev.type = ev

  val adhl = the[Fun[A]]

  // Compiles in scalac: the tagged case wins the implicit search using A.tag
  // Does not compile in Dotty: because of Tagged.Aux[T, _] the companion
  //                            object of T is not explored during the search,
  //                            it fallbacks to default (type Out = String)
  identity[Fun[A] { type Out = Int }](adhl)
}
