object opaque {
  opaque type Foo[X] <: String = String
}
object test {
  val s: String = ???.asInstanceOf[opaque.Foo[String]]
}
object opaque2 {
  opaque type Foo2 <: String = String
}
object test2 {
  val s: String = "bla"
}