type Person = (name: String, age: Int)

val so = summon[Ordering[Person]]

val people: List[Person] = List(
  (name = "Charlie", age = 35),
  (name = "Alice", age = 30),
  (name = "Alice", age = 29),
  (name = "Bob", age = 25),
)
val sortedPeople = people.sorted

@main def Test = println(s"$sortedPeople")
