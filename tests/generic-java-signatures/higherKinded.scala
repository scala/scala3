import scala.language.higherKinds

class Foo[t[x], u[y] <: java.util.List[y], v[z[a] <: Array[a]], b[B] <: java.util.List[B], w[b[c]] <: java.util.HashMap[Array[Int], b[java.util.Date]]]
object Test {
  def main(args: Array[String]): Unit = {
    val tParams = classOf[Foo[_, _, _, _, _]].getTypeParameters
    tParams.foreach { tp =>
      val tp1 = tp.nn
      println(tp1.getName + " <: " + tp1.getBounds.map(_.nn.getTypeName).mkString(", "))
    }
  }
}
