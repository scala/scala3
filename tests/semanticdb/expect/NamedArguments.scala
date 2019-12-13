package example

class NamedArguments {
  case class User(name: String)
  User(name = "John")
  User.apply(name = "John")
}
