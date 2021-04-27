import scala.quoted.*
import scala.quoted.staging.*
object Test {
  given Compiler = Compiler.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = println(run {
    val a = Expr(BigDecimal(2.3))
    val b = Expr(BigDecimal("1005849025843905834908593485984390583429058574925.95489543"))
    '{ ($a, $b) }
  })
}
