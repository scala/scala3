// scalajs: --skip
object Test {
  def main(args: Array[String]): Unit = {
    val v = dotty.tools.dotc.config.Properties.versionNumberString
    assert(v.nonEmpty && v.startsWith("3."))
  }
}
