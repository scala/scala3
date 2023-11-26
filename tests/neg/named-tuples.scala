import annotation.experimental
import language.experimental.namedTuples

@experimental object Test:

  type Person = (name: String, age: Int)
  val person = (name = "Bob", age = 33): (name: String, age: Int)

  type NameOnly = (name: String)

  val nameOnly = (name = "Louis")

  val y: (String, Int) = person // error
  val _: NameOnly = person // error
  val _: Person = nameOnly // error

  val _: (age: Int, name: String) = person // error

  ("Ives", 2) match
    case (name = n, age = a) => () // error // error

  val pp = person ++ (1, 2)  // ok, but should also be error
  val qq = ("a", true) ++ (1, 2)

  person ++ (1, 2) match // error
    case _ =>

  val bad = ("", age = 10) // error

  person match
    case (name = n, age) => () // error
    case (name, age = a) => () // error
