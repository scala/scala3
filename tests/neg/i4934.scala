object Foo {
  val a = ""); // error: end of statement expected
}
// From #5824:
object Main {
  def main(args: Array[String]): Unit = {
    val foo = 123 ""; // error: end of statement expected
    println(foo)
  }
}