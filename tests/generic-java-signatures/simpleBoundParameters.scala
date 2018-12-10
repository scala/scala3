class Foo[T <: java.util.List[_]]
object Test {
  def main(args: Array[String]): Unit = {
    val tParams = classOf[Foo[_]].getTypeParameters()
    tParams.foreach { tp =>
      val tp1 = tp.nn
      println(tp1.getName + " <: " + tp1.getBounds.map(_.nn.getTypeName).mkString(", "))
    }
  }
}
