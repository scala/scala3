class Foo[
  T <: Array[?],
  U <: Array[T],
  V <: java.util.List[Array[T]],
  W <: java.util.List[? <: java.util.Date],
  X <: java.util.HashMap[Array[?], java.util.ArrayList[? <: java.util.Date]],
  T1,
  U1 <: Array[T1],
  V1 <: Array[Array[T1]]
]
object Test {
  def main(args: Array[String]): Unit = {
    val tParams = classOf[Foo[?, ?, ?, ?, ?, ?, ?, ?]].getTypeParameters()
    tParams.foreach { tp =>
      println(tp.getName + " <: " + tp.getBounds.map(_.getTypeName).mkString(", "))
    }
  }
}
