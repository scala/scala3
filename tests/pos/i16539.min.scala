//> using options -Werror
sealed trait Tag[A]

sealed trait Foo
case class Bar[A](fn: A => Unit) extends Foo, Tag[A]

class Test:
  def pmat[A](sel: Foo & Tag[A]): Unit = sel match
    case Bar(_) =>
