enum Option[+T] extends Serializable {
  case Some[T](x: T) extends Option[T]
  case None

  def isDefined: Boolean = this match {
    case None => false
    case some => true
  }
}
object Option {
  def apply[T >: Null](x: T): Option[T] = if (x == null) None else Some(x)
}

object Test {
  import Option.*
  def main(args: Array[String]) = {
    assert(Some(None).isDefined)
    Option("22") match { case Option.Some(x) => assert(x == "22") }
    assert(Some(None) != None)
  }
}
