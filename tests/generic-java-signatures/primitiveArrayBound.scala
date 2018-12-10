class Foo[T <: Array[_ <: Int]]
object Test {
  def main(args: Array[String]): Unit = {
    val tParams = classOf[Foo[_]].getTypeParameters
    tParams.foreach { tp =>
      val tp1 = tp.nn
      println(tp1.getName.nn + " <: " + tp1.getBounds.map(_.nn.getTypeName).mkString(", "))
    }
  }
}
