import language.experimental.captureChecking

trait Builder[-A, +C]
trait BuildFrom[-From, -A, +C] {
  def newBuilder(from: From): Builder[A, C]
}

trait Future[+T] {
  import Future.*
  def foldLeft[R](r: R): R = r
  def traverse[A, B, M[X] <: IterableOnce[X]](in: M[A]^, bf: BuildFrom[M[A]^, B, M[B]^]): Unit =
    val builder = bf.newBuilder(in)
    foldLeft(successful(builder))
}
object Future {
  def successful[T](result: T): Future[T] = ???
}
