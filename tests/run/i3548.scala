trait Common {
  case class Data(a: String)
}
object O1 extends Common
object O2 extends Common

object Test {
  def main(args: Array[String]): Unit = {

    val data = O2.Data("test")

    val result = data match {
      case O1.Data(s) => 1
      case O2.Data(s) => 2
      case _ => 3
    }

    assert(result == 2)
  }
}