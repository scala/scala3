class T1 {
  case class Foo(x: Int, xs1: List[String], xs2: List[String])
}

object T2 {
  val foo: T1#Foo = ???

  val Foo(x1, xs1, xs2) = foo // error
}
