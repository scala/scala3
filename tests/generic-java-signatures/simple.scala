class Foo[T, U, LongerName]
object Test {
  def main(args: Array[String]): Unit = {
    val typeParams = classOf[Foo[_, _, _]].getTypeParameters
    typeParams.foreach(tp => println(tp.getName))
  }
}