import language.experimental.namedTuples
import scala.annotation.experimental

@experimental object Test:

  case class PersonCaseClass(name: String, age: Int)

  val personCaseClass = PersonCaseClass("Bob", 33)
  personCaseClass match
    case PersonCaseClass(name = n, age) => () // error
    case PersonCaseClass(name, age = a) => () // error

  val person = (name = "Bob", age = 33): (name: String, age: Int)
  person match
    case (name = n, age) => () // error
    case (name, age = a) => () // error
