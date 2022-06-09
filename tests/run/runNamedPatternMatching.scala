object Test:

  class User(val name: String, val age: Int, val city: String)

  //TODO: Test this
  object UserEx:
    class UserExtractor(user: User) extends Product:
      def _1 = println("Got name"); user.name
      def _2 = println("Got age"); user.age
      def _3 = println("Got city"); user.city

      type Names = ("name", "age", "city")

      // Members declared in scala.Equals
      def canEqual(that: Any): Boolean = ???

      // Members declared in scala.Product
      def productArity: Int = ???
      def productElement(n: Int): Any = ???

    def unapply(user: User) = UserExtractor(user)

  def main(args: Array[String]): Unit =
    val UserEx(city = _, name = _) = User("Guy", 25, "Paris")
