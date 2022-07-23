object Test1:
  class User(val name: String, val age: Int, val city: String)
  object User:
    def unapply(user: User) = UserExtractor(user.name, user.age, user.city)

  case class UserExtractor(name: String, age: Int, city: String)

  class Person(val name: String)
  object Person:
    def unapply(person: Person) = PersonExtractor(person.name)

  case class PersonExtractor(name: String)

  def test =
    val user = User("Bob", 22, "Paris")
    (user: Any) match
      case User(name, age, city) => println(s"$name is $age years old and lives in $city")
    val p = Person("Peter")
    (p: Any) match
      case Person(n) => println(s"Hello $n")

object Test2:
  class User(val name: String, val age: Int, val city: String)
  object User:
    def unapply(user: User): Option[UserExtractor] = Some(UserExtractor(user.name, user.age, user.city))

  case class UserExtractor(name: String, age: Int, city: String)

  class Person(val name: String)
  object Person:
    def unapply(person: Person): Some[PersonExtractor] = Some(PersonExtractor(person.name))

  case class PersonExtractor(name: String)

  def test =
    val user = User("Bob", 22, "Paris")
    (user: Any) match
      case User(name, age, city) => println(s"$name is $age years old and lives in $city")
    val p = Person("Peter")
    (p: Any) match
      case Person(n) => println(s"Hello $n")

@main def Test =
  Test1.test
  Test2.test
