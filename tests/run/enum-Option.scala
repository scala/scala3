enum class Option[+T] extends Serializable {
   def isDefined: Boolean
}
object Option {
  case Some(x: T) {
     def isDefined = true
  }
  case None extends Option[Nothing] {
     def isDefined = false
  }
}

object Test {
  def main(args: Array[String]) =
    assert(Some(None).isDefined)
}
