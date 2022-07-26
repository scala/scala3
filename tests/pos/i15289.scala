// scalac: -Werror
sealed abstract class Foo[A, B]
final case class Bar[C](baz: C) extends Foo[C, C]

class Test:
  def m1[X](f1: Foo[X, String]): String = f1 match { case Bar(_) => "" }
