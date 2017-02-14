enum class Option[+T] extends Serializable {
   def isDefined: Boolean
}
object Option {
  def apply[T](x: T): Option[T] = if (x == null) None else Some(x)
  case Some(x: T) {
     def isDefined = true
  }
  case None extends Option[Nothing] {
     def isDefined = false
  }
}

object Test {
  def main(args: Array[String]) = {
    assert(Some(None).isDefined)
    Option(22) match { case Option.Some(x) => assert(x == 22) }
  }
}
