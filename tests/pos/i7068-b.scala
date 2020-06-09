inline def species() = {
  case class FooT()
  FooT()
}
val foo = species()
