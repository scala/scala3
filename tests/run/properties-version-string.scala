object Test {
  def main(args: Array[String]): Unit = {
    val v = scala.util.Properties.versionNumberString
    if (v.nonEmpty && !v.startsWith("2.")) println("OK")
    else println("FAIL " + v)
  }
}
