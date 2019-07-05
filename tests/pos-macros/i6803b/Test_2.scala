import blah._

object Test {
  def main(args: Array[String]): Unit = {

    def testO(): Unit = {
      import AsObject.LineNo
      the[LineNo] given LineNo.x
    }
  }
}
