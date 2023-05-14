// scalajs: --skip

object Test {

  def main(args: Array[String]): Unit = {
    try
      assert(false, "my message")
    catch
      case ae: AssertionError => printStack(ae)

    try
      assert(false)
    catch
      case ae: AssertionError => printStack(ae)
  }

  def printStack(ae: AssertionError): Unit =
    for elem <- ae.getStackTrace().iterator.takeWhile(x => !(x.getMethodName == "main" && x.getLineNumber == -1)) do
      println(elem)
    println()

}
