import scala.language.experimental.namedTuples

def f(g: Int  => Unit) = g(0)

def test =
  var cache: Option[Int] = None
  f(i => (cache = Some(i))) // warn
