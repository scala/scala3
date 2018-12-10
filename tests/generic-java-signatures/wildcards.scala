class Foo[T <: java.util.List[_ <: java.util.Calendar], U <: java.util.HashMap[java.util.List[_ <: T], java.util.Set[_ >: T]], V <: java.util.Queue[Int]]
object Test {
  def main(args: Array[String]): Unit = {
    val tParams = classOf[Foo[_, _, _]].getTypeParameters
    tParams.foreach { tp =>
      val tp1 = tp.nn
      println(tp1.getName + " <: " + tp1.getBounds().map(_.nn.getTypeName).mkString(", "))
    }
  }
}
