class Foo[T <: Array[_], U <: Array[T], V <: java.util.List[Array[T]], W <: java.util.List[_ <: java.util.Date], X <: java.util.HashMap[Array[_], java.util.ArrayList[_ <: java.util.Date]]]
object Test {
  def main(args: Array[String]): Unit = {
    val tParams = classOf[Foo[_, _, _, _, _]].getTypeParameters()
    tParams.foreach { tp =>
      val tp1 = tp.nn
      println(tp1.getName + " <: " + tp1.getBounds.map(_.nn.getTypeName).mkString(", "))
    }
  }
}
