// scalac: -Werror
sealed abstract class Foo[N, A]
final case class Bar[B](foo: Foo[B, B]) extends Foo[B, B]
class Test:
  def pmat[P, C](scr: Foo[P, C]): C = scr match
    case Bar(foo) => pmat(foo)
