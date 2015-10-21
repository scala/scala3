import scala.reflect._
// this needs to be fleshed out further
class Contra[-T]

object Test {
  def getParam[T](c: Contra[T])(implicit ct: ClassTag[T]): Unit = {
    println(ct)
    ct
  }
  def f[T](x: Contra[T]): Contra[T] = x

  def main(args: Array[String]): Unit = {
    val x = f(new Contra[Int])
    val y: Contra[Int] = x
    getParam(new Contra[Int])
  }
}

