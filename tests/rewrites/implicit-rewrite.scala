//> using options source `future-migration` -rewrite

class Ord[T]

object Test:

  implicit def ol[T](implicit x: Ord[T]): Ord[List[T]] = foo[T]

  def foo[T](implicit x: Ord[T]): Ord[List[T]] = new Ord[List[T]]()
