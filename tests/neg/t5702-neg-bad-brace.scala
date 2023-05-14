
object Test {

  def main(args: Array[String]) = {
    val is = List(1,2,3)

    is match {
      case List(1, _*} => // error: pattern expected
    }
  }
} // error: eof expected, but '}' found
