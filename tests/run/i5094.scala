trait IO {
  def c(x: Int): Int = ???
}
trait SO extends IO {
  override final def c(x: Int): Int = ???
}
trait SOIO extends IO {
  override def c(x: Int): Int = ???
}
trait SOSO extends SOIO with SO
abstract class AS extends SO
class L extends AS with SOSO
object Test {
  def main(args: Array[String]): Unit = {
    new L
  }
}
