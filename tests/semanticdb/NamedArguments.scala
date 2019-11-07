package example

class NamedArguments {
  case class User(name: String)
  User(name = "John") // as there is no API to get the symbols of method arguments, this proposes a problem for how to generate references from a NamedArg
  User.apply(name = "John") // as there is no API to get the symbols of method arguments, this proposes a problem for how to generate references from a NamedArg
}
