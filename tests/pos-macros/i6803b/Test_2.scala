import blah._

object Test {
  def main(args: Array[String]): Unit = {

    def testO(): Unit = {
      import AsObject.LineNo
      summon[LineNo](given LineNo.x)
    }
  }
}
