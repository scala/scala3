/**
 * Doc of Test
 * */
class Test

object Test {
  def main(args: Array[String]): Unit = {
    println("docs from same compilation run:")
    Main.printdoc()

    println("docs loaded from tasty:")
    val docString = Doc.of[Main]
    println(docString)

    println("docs from same compilation run:")
    val docStringTest = Doc.of[Test]
    println(docStringTest)
  }
}
