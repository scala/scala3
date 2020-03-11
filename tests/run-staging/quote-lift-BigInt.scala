import scala.quoted._
import scala.quoted.staging._
object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = println(run {
    val a = Lifted(BigInt(2))
    val b = Lifted(BigInt("1005849025843905834908593485984390583429058574925"))
    '{ ($a, $b) }
  })
}
