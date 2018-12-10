class Foo[T <: java.util.List[_], U <: java.util.ArrayList[java.util.Date], V <: java.util.ArrayList[java.util.HashMap[java.util.HashSet[_], java.util.Calendar]]]
object Test {
  def main(args: Array[String]): Unit = {
    val tParams = classOf[Foo[_, _, _]].getTypeParameters()
    tParams.foreach { tp =>
      val tp1 = tp.nn
      println(tp1.getName + " <: " + tp1.getBounds.map(_.nn.getTypeName).mkString(", "))
    }
  }
}
