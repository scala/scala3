import scala.quoted._
import scala.quoted.staging._
object Test {
  delegate for Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = println(run {
    val a = BigDecimal(2.3).toExpr
    val b = BigDecimal("1005849025843905834908593485984390583429058574925.95489543").toExpr
    '{ ($a, $b) }
  })
}
