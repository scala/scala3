import scala.quoted._
object Test {
  val tb = Toolbox.make
  def main(args: Array[String]): Unit = {
    tb.run {
      var e = '(1)
      e = '(~e + 1)
      val res = e.show
      '(println(~res.toExpr))
    }
  }
}
