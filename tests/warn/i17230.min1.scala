//> using options -Werror
trait Foo:
  type Bar[_]

object Foo:
  type Aux[B[_]] = Foo { type Bar[A] = B[A] }

class Test:
  def t1[B[_]](self: Option[Foo.Aux[B]]) = self match
    case Some(_) => 1
    case None    => 2

  def t2[B[_]](self: Option[Foo.Aux[B]]) = self match
    case Some(f) => 1
    case None    => 2
