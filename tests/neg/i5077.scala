//> using options -Xfatal-warnings -Wimplausible-patterns
trait Is[A]
case object IsInt extends Is[Int]
case object IsString extends Is[String]
case class C[A](is: Is[A], value: A)

@main
def Test = {
  val c_string: C[String] = C(IsString, "name")
  val c_any: C[?] = c_string
  val any: Any = c_string

  // Case 1: error
  c_string match {
    case C(IsInt, _) => println(s"An Int") // error
    case C(IsString, s) => println(s"A String with length ${s.length}")
    case _ => println("No match")
  }

  // Case 2: Should match the second case and print the length of the string
  c_any match {
    case C(IsInt, i) if i < 10 => println(s"An Int less than 10")
    case C(IsString, s) => println(s"A String with length ${s.length}")
    case _ => println("No match")
  }

  // Case 3: Same as above; should match the second case and print the length of the string
  any match {
    case C(IsInt, i) if i < 10 => println(s"An Int less than 10")
    case C(IsString, s) => println(s"A String with length ${s.length}")
    case _ => println("No match")
  }
}