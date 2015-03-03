package p {
  class C[A] { def foo: A = ??? }

  object `package` extends C[String]
}

object test {

  val x: String = p.foo

}
