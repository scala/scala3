class Foo[T <: java.io.Serializable, U >: java.lang.Cloneable]
object Test {
  def main(args: Array[String]): Unit = {
    val tParams = classOf[Foo[_, _]].getTypeParameters
    tParams.foreach { tp =>
      println(tp.getName + " <: " + tp.getBounds.map(_.getTypeName).mkString(", "))
    }
  }
}