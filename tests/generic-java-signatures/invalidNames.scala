class Foo[`![]:;!!` <: java.util.Date]

object Test {
  def main(args: Array[String]): Unit = {
    val tParams = classOf[Foo[_]].getTypeParameters()
    tParams.foreach { tp =>
      println(tp.getName + " <: " + tp.getBounds.map(_.getTypeName).mkString(", "))
    }
  }
}
