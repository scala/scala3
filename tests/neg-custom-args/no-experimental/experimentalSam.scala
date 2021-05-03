import scala.annotation.experimental

@experimental // FIXME ERROR
trait ExpSAM {
  def foo(x: Int): Int
}
def bar(f: ExpSAM): Unit = {} // error

def test: Unit =
  bar(x => x) // error
  ()
