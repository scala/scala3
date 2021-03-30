import java.util.function.Function

class Future[T](val initial: T) {
  def map[V](v: V): Unit = println(v)
  //def map(v: String): Unit = println(v)
  def map[U](fn: Function[T, U]): Unit = println(fn(initial))
}

object Test {
  val f = new Future(42)
  val fn = (i: Int) => i.toString
  def main(args: Array[String]): Unit =
    f.map((i: Int) => i.toString)
}