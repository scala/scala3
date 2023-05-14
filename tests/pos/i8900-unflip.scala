// Minimized from PLens.scala in scalaz

class PLensFamily[A1, A2, B1, B2]

class LazyOption[A3] {
  def fold[X](some: (=> A3) => X, none: => X): X = ???
}
class IndexedStore[I, A4, B4](run: (A4 => B4, I))

object PL {

  def plensFamily[A1x, A2x, B1x, B2x](r: A1x => Option[IndexedStore[B1x, B2x, A2x]]): PLensFamily[A1x, A2x, B1x, B2x] = ???

  def lazySome[T](a: => T): LazyOption[T] = ???

  def lazySomePLensFamily[A1y, A2y]: PLensFamily[LazyOption[A1y], LazyOption[A2y], A1y, A2y] =
    plensFamily(_.fold(z => Some(IndexedStore(lazySome(_), z)), None))
}
