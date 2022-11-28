object Meow extends App {
  case class Meow(s: String, i: Int)

  val dir: Directive[EmptyTuple] = ???
  dir {
    Meow("asd", 123)
  }
}
