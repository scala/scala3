class Foo[T <: java.util.Calendar, U >: java.util.Date]
object Test {
  def main(args: Array[String]): Unit = {
    val tParams = classOf[Foo[_, _]].getTypeParameters
    tParams.foreach { tp =>
      println(tp.nn.getName + " <: " + tp.nn.getBounds.map(_.nn.getTypeName).mkString(", "))
    }
  }
}
