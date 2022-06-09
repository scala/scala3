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

  object Helper:
    val empty: EmptyTuple & { type Names = EmptyTuple } = EmptyTuple.asInstanceOf

    extension [N <: Singleton & String, V] (head: (N, V))
      def +::[Vs <: Tuple, Ns <: Tuple](tail: Vs & { type Names = Ns }) : V *: Vs & { type Names = N *: Ns }=
        (head._2 *: tail).asInstanceOf

  object UserEx2:
    import Helper._
    // TODO: The inferred type doesn't work and leads to a runtime exception
    def unapply(user: User): (String, Int, String) & { type Names = ("name", "age", "city") } =
      ("name", user.name) +:: ("age", user.age) +:: ("city", user.city) +:: empty

  def main(args: Array[String]): Unit =
    val UserEx(city = _, name = _) = User("Guy", 25, "Paris")
    val UserEx2(city = _, name = n) = User("Guy", 25, "Paris")

    println(n)
