// Test that we correctly handle scrutinees with type `Null` or `Nothing`.
object Test {
  def main(args: Array[String]): Unit = {
    null match {
      case Some(_) => println("matches Some")
      case (_, _) => println("matches Pair")
      case null => println("matches null literal")
    }

    type X = Null
    (null: X) match {
      case Some(_) => println("matches Some")
      case (_, _) => println("matches Pair")
      case null => println("matches null literal")
    }

    type Y = Nothing
    try {
      (??? : Y) match {
        case Some(_) => println("matches Some")
        case _ => println("matches anything")        
      }
    } catch {
      case e: NotImplementedError => println("not implemented")
    }


    try {
      val Some(_) = null
    } catch {
      case e: MatchError => println("match error")
    }

    try {
      val Some(_) = ???
    } catch {
      case e: NotImplementedError => println("not implemented")
    }
    
    val x: X = null
    try {
      val Some(_) = x
    } catch {
      case e: MatchError => println("match error")
    }
  }
}
