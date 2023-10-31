class Foo[T <: java.util.List[?], U <: java.util.ArrayList[java.util.Date], V <: java.util.ArrayList[java.util.HashMap[java.util.HashSet[?], java.util.Calendar]]]
object Test {
  def main(args: Array[String]): Unit = {
    val tParams = classOf[Foo[?, ?, ?]].getTypeParameters()
    tParams.foreach { tp =>
      println(tp.getName + " <: " + tp.getBounds.map(_.getTypeName).mkString(", "))
    }
  }
}