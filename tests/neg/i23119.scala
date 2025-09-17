//> using options -explain

@main def test = println:
  for x <- 1 to 2
    // works with explicit name
    //ols @ given Option[List[String]] = Some(List(x.toString))
    given Option[List[String]] = Some(List(x.toString))
    given Option[List[Int]] = Some(List(x)) // error
  yield summon[Option[List[String]]].map(ss => ss.corresponds(given_Option_List.get)((a, b) => a == b.toString))

// The naming clash is noticed when defining local values for "packaging":
//  given_Option_List is already defined as given instance given_Option_List
// Previously the naming clash was noticed when extracting values in the map or do function:
//  duplicate pattern variable: given_Option_List

def also =
  given [A] => List[A] = ???
  given [A] => List[A] = ??? // error
  ()
