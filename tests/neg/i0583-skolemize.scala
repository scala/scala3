import scala.collection.mutable.ListBuffer

object Test1 {

  class Box[T](x: T)
  def box[T](x: T) = new Box[T](x)

  class C[T] { def x: T = ???; def x_=(y: T): Unit = ??? }

  val c: C[Int] = ???
  val d: C[Float] = ???

  val xs: List[C[_]] = List(c, d)

  xs(0).x = xs(1).x                                   // error

}
object Test {
  def main(args: Array[String]): Unit = {
    val f: ListBuffer[Int] = ListBuffer(1,2)
    val g: ListBuffer[Double] = ListBuffer(3.0,4.0)
    val lb: ListBuffer[ListBuffer[_]] = ListBuffer(f, g)
    lb(0)(0) = lb(1)(0)                               // error
    val x: Int = f(0)
  }
}

