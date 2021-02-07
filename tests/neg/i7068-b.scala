inline def species() = {
  case class FooT() // error
  FooT()
}
val foo = species()
