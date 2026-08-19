//> using options -Yexplicit-nulls
import language.experimental.magic
object TTest:
  def unapplySeq(t: Int): Seq[Int]? = Seq(1, 2)

case class Varargs(xs: Int*)

def test =
  1 match
    case TTest(x*) => ()

  1 match
    case TTest(_*) => ()

  1 match
    case TTest(1, rest*) => ()
    case TTest(_*) => ()

  Varargs(1, 2, 3) match
    case Varargs(x*) => ()

  Varargs(1, 2, 3) match
    case Varargs(1, rest*) => ()
    case Varargs(_*) => ()
