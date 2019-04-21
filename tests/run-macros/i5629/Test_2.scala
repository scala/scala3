object Test {
  import Macros._

  def main(args: Array[String]): Unit = {
    val startLine = thisLineNumber
    assert(thisLineNumber == startLine + 1)
    assert(thisLineNumber == startLine + 2)
    scala.Predef.assert(thisLineNumber == startLine + 3)
  }
}
