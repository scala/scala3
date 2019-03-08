// Test that we correctly handle scrutinees with type `Null` or `Nothing`.
object Test {
  def main(args: Array[String]): Unit = {
      class B[T] {}
      object B {
        def unapply[T](x: Any): Option[B[T]] = None
      }
      try {
        val B(_) = null
      } catch {
        case e: MatchError => println("match error")
      }

      null match {
        case null =>
          try {
            null match {
              case Some(_) => ()
            }
          } catch {
            case e: MatchError => println("match error nested")
          }
      }

      try {
        ??? match {
          case (_, _) => ()
          case _ => ()
        }
      } catch {
        case e: NotImplementedError => println("not implemented error")
      }
  }
}
