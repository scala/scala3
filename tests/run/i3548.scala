trait Common {
  case class Data(a: String)
}
object O1 extends Common
object O2 extends Common

object Test {
  def main(args: Array[String]): Unit = {

    val data = O2.Data("test")

    // Runtime error: java.lang.ClassCastException: O2$ cannot be cast to O1$
    data match {
        case O1.Data(s) => println("O1 data")
        case O2.Data(s) => println("O2 data")
        case _ => println("Unknown")
    }
  }
}