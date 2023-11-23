import NamedTuple.dropNames

type Person = (name: String, age: Int)
val bob = (name = "Bob", age = 33): (name: String, age: Int)
val person2: (name: String, age: Int) = bob

type Uni = (uni: Double)
val uni = (uni = 1.0)
val _: Uni = uni

type AddressInfo = (city: String, zip: Int)
val addr = (city = "Lausanne", zip = 1003)
val _: AddressInfo = addr

type CombinedInfo = Tuple.Concat[Person, AddressInfo]
val bobWithAddr = bob ++ addr
val _: CombinedInfo = bobWithAddr
val _: CombinedInfo = bob ++ addr

@main def Test =
  println(bob)
  println(bob.age)
  println(person2.name)
  println(bobWithAddr)
  bob match
    case p @ (name = "Bob", age = _) => println(p.age)
  bob match
    case p @ (name = "Peter", age = _) => println(p.age)
    case p @ (name = "Bob", age = 0) => println(p.age)
    case _ => println("no match")

  val x = bob.age
  assert(x == 33)

  val y: (String, Int) = bob.dropNames

  def ageOf(person: Person) = person.age

  assert(ageOf(bob) == 33)
  assert(ageOf((name = "anon", age = 22)) == 22)
  assert(ageOf(("anon", 11)) == 11)


