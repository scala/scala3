package tests.rightAssocExtension

case class Wrap[+T](x: T)

extension [T](a: T)
  def *:[U <: Tuple](b: Wrap[U]): Wrap[T *: U]
    = Wrap(a *: b.x)
