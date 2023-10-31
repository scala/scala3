class Foo[T <: java.util.List[? <: java.util.Calendar], U <: java.util.HashMap[java.util.List[? <: T], java.util.Set[? >: T]], V <: java.util.Queue[Int]]
object Test {
  def main(args: Array[String]): Unit = {
    val tParams = classOf[Foo[?, ?, ?]].getTypeParameters
    tParams.foreach { tp =>
      println(tp.getName + " <: " + tp.getBounds().map(_.getTypeName).mkString(", "))
    }
  }
}