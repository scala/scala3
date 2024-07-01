class Person(
    val firstName: String,
    val lastName: String,
    val birthYear: Int = -1,
    val address: String = ""
):
  // Works if remove this constructor
  def this() = this("John", "Doe")

class Test:
  def p1 = Person("First", "Last") // was: Type Error: none of the overloads.. match arguments
  def p2 = Person("Josh", "Joe", 1912, "Main Street")
