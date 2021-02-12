import scala.quoted.*
import scala.quoted.staging.*
object Test {
  given Compiler = Compiler.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = println(run {
    val a = Expr(BigInt(2))
    val b = Expr(BigInt("1005849025843905834908593485984390583429058574925"))
    '{ ($a, $b) }
  })
}
