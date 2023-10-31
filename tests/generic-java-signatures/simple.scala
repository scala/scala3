class Foo[T, U, LongerName]
object Test {
  def main(args: Array[String]): Unit = {
    val typeParams = classOf[Foo[?, ?, ?]].getTypeParameters
    typeParams.foreach(tp => println(tp.getName))
  }
}