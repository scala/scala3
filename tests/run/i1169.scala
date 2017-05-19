
object Test {
  def main(args: Array[String]): Unit = {
    val x = new XXX
    println(x.yyyy("zzz").x)
  }
}

class A(val x: Object) extends AnyVal
abstract class VCACompanion[T] {
  def yyyy(x: Object): T
}
class XXX extends VCACompanion[A] {
  override def yyyy(x: Object): A = new A("fff")
}
