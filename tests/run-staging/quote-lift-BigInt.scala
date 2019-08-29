import scala.quoted._
import scala.quoted.staging._
object Test {
  delegate for Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = println(run {
    val a = BigInt(2).toExpr
    val b = BigInt("1005849025843905834908593485984390583429058574925").toExpr
    '{ ($a, $b) }
  })
}
