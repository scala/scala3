//> using options -Yexplicit-nulls
import language.experimental.magic
object Foo:
  def unapply(n: Int)(using x: DummyImplicit)(using y: Int): Int? = ???

def test =
  given Int = 3
  1 match
    case Foo(_) =>
