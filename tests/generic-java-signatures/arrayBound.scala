class Foo[T <: Array[_], U <: Array[T], V <: java.util.List[Array[T]], W <: java.util.List[_ <: java.util.Date], X <: java.util.HashMap[Array[_], java.util.ArrayList[_ <: java.util.Date]]]
object Test {
  def main(args: Array[String]): Unit = {
    val tParams = classOf[Foo[_, _, _, _, _]].getTypeParameters()
    tParams.foreach { tp =>
      println(tp.getName + " <: " + tp.getBounds.map(_.getTypeName).mkString(", "))
    }
  }
}