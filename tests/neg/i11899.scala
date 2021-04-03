import java.util.function.Function

class Future[T](val initial: T) {
  def map[V](v: V): Unit = println(v)
  def map[U](fn: Function[T, U]): Unit = println(fn(initial))
}

val f = new Future(42)
val fn = (i: Int) => i.toString
def test = f.map(fn)  // error
