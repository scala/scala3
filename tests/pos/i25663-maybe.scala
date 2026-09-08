//> using options -Yexplicit-nulls
import language.experimental.errorHandling
object Foo:
  def unapplySeq(f: Int): (String *: Seq[Int] *: EmptyTuple)? = ???

def foo(f: Int) = f match
  case Foo(name, ns*) => ???
