// scalajs: --skip
object Test {
  def main(args: Array[String]): Unit = {
    val v = scala.util.Properties.versionNumberString
    assert(v.nonEmpty && v.startsWith("3."))
  }
}
