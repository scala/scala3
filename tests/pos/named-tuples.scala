type P = (Int, Int)
type Person = (name: String, age: Int)
val person = (name = "Bob", age = 33): (name: String, age: Int)/*
val person2: (name: String, age: Int) = person

@main def Test =
  println(person)
  println(person.age)
  println(person2.name)
  person match
    case p @ (name = "Bob", age = _) => println(p.age)
  person match
    case p @ (name = "Peter", age = _) => println(p.age)
    case p @ (name = "Bob", age = 0) => println(p.age)
    case _ => println("no match")


*/