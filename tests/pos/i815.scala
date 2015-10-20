import scala.collection.immutable.::
class C[T](x: T)
object A {
  def main(args: Array[String]): Unit = {
    val x = new C("A")
    val y = new ::(args, Nil)
    val z = ::(args, Nil)
    println(y)
    println(z)
  }
}
