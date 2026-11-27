package example

object NamedTuples:
  type Person = (name: String, age: Int)
  val x: Person = (name = "a", age = 1)
  val y = x.name
  val z = x.apply(0)
  val w = x match
    case (name = n, age = a) => n
  val v = x.name.length

  case class P(name: String)
  val p: NamedTuple.From[P] = ???
  val q = p.name
