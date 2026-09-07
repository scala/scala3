import language.experimental.captureChecking

trait Repro[A]:
  def toIterable: Iterable[A]^{this}

  def iterate(f: Iterable[A]^{this} => Iterable[A]^{this}) =
    Iterator.iterate(toIterable)(f)
