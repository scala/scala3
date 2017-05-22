enum class Option[+T >: Null] extends Serializable {
   def isDefined: Boolean
}
object Option {
  def apply[T >: Null](x: T): Option[T] = if (x == null) None else Some(x)

  case Some[+T >: Null](x: T) {
     def isDefined = true
  }
  case None {
     def isDefined = false
  }
}

object Test {
  def main(args: Array[String]) = {
    assert(Some(None).isDefined)
    Option("22") match { case Option.Some(x) => assert(x == "22") }
  }
}
