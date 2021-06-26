object wrapper {
  val / = "slash"
}

object foo {
  implicit val postfixOps: scala.languageFeature.postfixOps = scala.language.postfixOps
  val / = wrapper./
}

object Test {
  def main(args: Array[String]): Unit = {
    val wd = foo./
    println(wd)
  }
}
