trait TC[X]
object TC {
  given t2[T]: TC[T] = ???
  given t1[T]: TC[T] = ???
}
class Seq2[T] {
  def sorted(using TC[T]): Seq2[T] = ???
}
extension [T](s: Seq2[T])
def f[G](c: Seq2[G]): Unit = ??? // error // Extension without extension methods
object Crash {
  val s: Seq2[Int] = ???
  s.sorted.f(Seq2()) // error // ambiguous given instances: both given instance t2 in object TC and given instance t1 in object TC match type TC[Int] of parameter x$1 of method sorted in class Seq2
}
