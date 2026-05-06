class A
object A:
  given g[F[_]]: F[A] = ???

object Test:
  summon[List[A]] // ok
  def foo[F[_]] =
    summon[F[A]] // error

final case class X(val i: Int)
object X {
  implicit final class XOps[F[_]](xs: F[X]) {
    def unpack(implicit ev: F[X] <:< Iterable[X]): Iterable[Int] = xs.map(_.i)
  }
}

object App extends App {
  // good
  val ys: List[X] = List(X(1))
  println(ys.unpack)

  // bad
  def printPolymorphic[F[_]](xs: F[X])(implicit ev: F[X] <:< Iterable[X]) = {
    locally {
      // implicit XOps is correct
      import X.XOps
      println(xs.unpack) // found
    }
    // but it's not being searched for in the companion object of X
    println(xs.unpack) // error: unpack is not a member of F[X]
  }
  printPolymorphic[List](ys)
}