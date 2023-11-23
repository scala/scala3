
type Person = (name: String, age: Int)
val person = (name = "Bob", age = 33): (name: String, age: Int)

type NameOnly = (name: String)

val nameOnly = (name = "Louis")



def Test =
  val y: (String, Int) = person // error
  val _: NameOnly = person // error
  val _: Person = nameOnly // error

  val _: (age: Int, name: String) = person // error
