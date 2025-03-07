import scala.language.experimental.namedTuples

type Person = (name: String, age: Int)

val Bob: Person = (name = "Bob", age = 33)

type SI = (String, Int)

def id[X](x: X): X = x
val x: (String, Int) = Bob
val y: SI = id(Bob)
val and: Person & String = ???
val _: SI = and
val or: Person | (name: "Bob", age: 33) = ???
val _: SI = or

class C[P <: Person](p: P):
  val x: (String, Int) = p




