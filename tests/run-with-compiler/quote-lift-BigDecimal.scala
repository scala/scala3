import scala.quoted._
object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = println(run {
    val a = BigDecimal(2.3).toExpr
    val b = BigDecimal("1005849025843905834908593485984390583429058574925.95489543").toExpr
    '{ ($a, $b) }
  })
}
