object Matcher {
  // Chained Match
  val chained_match_xs: List[Any] = List(1, 2, 3)
  val chained_match_x = chained_match_xs match {
    case Nil => "empty"
    case _   => "nonempty"
  } match {
    case "empty"    => 0
    case "nonempty" => 1
  }
  println(chained_match_x)

  // Vararg Splices
  val vararg_arr = Array(0, 1, 2, 3)
  val vararg_lst = List(vararg_arr*)                   // vararg splice argument
  // Throws an exception?
  val vararg_splice = vararg_lst match
    case List(0, 1, xs*) => 1  // binds xs to Seq(2, 3)
    case List(1, _*) => 0           // wildcard pattern
    case _ => 2
  println(vararg_splice)
  println(vararg_lst)

  // Pattern Definitions
  val patter_def_xs: List[Any] = List(1, 2, 3)
  val (patter_def_x: Any) :: _ = patter_def_xs : @unchecked
  println(patter_def_x)

  val patter_def_pair = (1, true)
  val (patter_def_a, patter_def_b) = patter_def_pair
  println(patter_def_a)

  val elems: List[(Int, Int)] = List((1, 2), (3, 4), (5, 6))

  for ((x,y) <- elems) do println(x)

  def main(args: Array[String]) = {
    // println(chained_match_x)
    println(vararg_splice)
    // println(patter_def_x)
    // println(
  }
}

// Patter Matching Using Extractors

// Option Extractors
case class Person(name: String, age: Int)
object Person {
  def unapply(person: Person): Option[(String, Int)] = Some((person.name, person.age))
}

object OptionMatcher {
  val person = Person("Alice", 25)

  val result = person match {
    case Person(name, age) => s"Name: $name, Age: $age"
    case _ => "Not a person"
  }
  println(result)
}

// Boolean Extractors
object Adult {
  def unapply(person: Person): Boolean = person.age >= 18
}

object BooleanMatcher {
  val person = Person("Charlie", 17)

  val adultResult = person match {
    case Adult() => s"${person.name} is an adult"
    case _ => s"${person.name} is not an adult"
  }

  println(adultResult)
}

// Variadic Extractors
// Add cases for exceptions
//
// Adding some warning test cases
//  -

object VariadicExtractor {
  // Define an unapply method that takes a List and returns an Option of Seq
  def unapplySeq[A](list: List[A]): Option[Seq[A]] = Some(list)
}

object PatternMatchExample {
  def describeList(list: List[Int]): String = list match {
    case VariadicExtractor(1, 2, rest @ _*) =>
      s"Starts with 1, 2 followed by: ${rest.mkString(", ")}"
    case VariadicExtractor(1, rest @ _*) =>
      s"Starts with 1 followed by: ${rest.mkString(", ")}"
    case VariadicExtractor(first, second, rest @ _*) =>
      s"Starts with $first, $second followed by: ${rest.mkString(", ")}"
    case VariadicExtractor(single) =>
      s"Only one element: $single"
    case VariadicExtractor() =>
      "Empty list"
    case _ =>
      "Unknown pattern"
  }

  // Test cases
  println(describeList(List(1, 2, 3, 4, 5))) // Output: Starts with 1, 2 followed by: 3, 4, 5
  println(describeList(List(1, 3, 4, 5)))    // Output: Starts with 1 followed by: 3, 4, 5
  println(describeList(List(2, 3, 4, 5)))    // Output: Starts with 2, 3 followed by: 4, 5
  println(describeList(List(1)))             // Output: Only one element: 1
  println(describeList(List()))              // Output: Empty list
}
