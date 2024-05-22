import scala.language.experimental.namedTuples

object Test:
  type NT = NamedTuple.Concat[(hi: Int), (bla: String)]
  def foo(x: NT) =
    x.hi // error
