
object Test { // error: Test cannot be instantiated since it has a member type Foo with possibly conflicting bounds Any <: ... <: Nothing
  type Foo >: Any <: Nothing
}
