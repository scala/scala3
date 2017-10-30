class Foo[T <: java.util.List[_]]
object Test {
  def main(args: Array[String]): Unit = {
    val tParams = classOf[Foo[_]].getTypeParameters()
    tParams.foreach { tp =>
      println(tp.getName + " <: " + tp.getBounds.map(_.getTypeName).mkString(", "))
    }
  }
}