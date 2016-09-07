case class MyTuple(a: Int, b: Int)

object Test {
  def main(args: Array[String]): Unit =
    try {
      val mt: MyTuple = null
      val MyTuple(a, b) = mt
    } catch {
      case e: MatchError => ()
    }
}
