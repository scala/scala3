class Foo[A <: Array[Byte],
B <: Array[Char],
C <: Array[Double],
D <: Array[Float],
E <: Array[Int],
F <: Array[Long],
G <: Array[Short],
H <: Array[Boolean],
I <: Array[_ <: Byte]]
object Test {
  def main(args: Array[String]): Unit = {
    val tParams = classOf[Foo[_, _, _, _, _, _, _, _, _]].getTypeParameters
    tParams.foreach { tp =>
      println(tp.getName + " <: " + tp.getBounds().map(_.getTypeName).mkString(", "))
    }
  }
}
