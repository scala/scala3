case class User(name: String)

case class RegisteredUser(id: String, data: User) {
  export data.*
}

@main def Test() =
  println(RegisteredUser("Id", User("Name"))) // RegisteredUser(Name)
  println(RegisteredUser("Id", User("Name")).canEqual(User("Name"))) // True
  // The rest works as expected
  println(RegisteredUser("Id", User("Name")) == User("Name")) // False
  println(RegisteredUser("Id", User("Name")).hashCode == User("Name").hashCode) // False
  println(RegisteredUser("Id", User("Name")).productArity == User("Name").productArity) // False
