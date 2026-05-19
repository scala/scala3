trait TC[X]
object TC {
  given t2[T]: TC[T] = ???
  given t1[T]: TC[T] = ???
}
class Seq2[T] {
  def sorted(using TC[T]): Seq2[T] = ???
}
extension [T](s: Seq2[T])
  def f[G](c: Seq2[G]): Unit = ???
object Crash {
  val s: Seq2[Int] = ???
  s.sorted.f(Seq2())
}