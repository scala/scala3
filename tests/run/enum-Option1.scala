enum Option1[+T] extends Serializable {
  case Some1(x: T)
  case None1

  def isDefined: Boolean = this match {
    case None1 => false
    case some => true
  }
}
object Option1 {
  def apply[T >: Null](x: T): Option1[T] = if (x == null) None1 else Some1(x)
}

object Test {
  import Option1._
  def main(args: Array[String]) = {
    assert(Some1(None1).isDefined)
    Option1("22") match { case Option1.Some1(x) => assert(x == "22") }
    assert(Some1(None1) != None1)
  }
}
