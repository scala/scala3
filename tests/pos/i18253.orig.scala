import compiletime.ops.int.Max

trait DFSInt[W <: Int]
trait Candidate[R]:
  type OutW <: Int
object Candidate:
  given [W <: Int, R <: DFSInt[W]] => Candidate[R]:
    type OutW = W

def foo[R](rhs: R)(using icR: Candidate[R]): DFSInt[Max[8, icR.OutW]] = ???

object Test:
  def check[A](a: A, clue: Int = 1): Unit = ???
  val x: DFSInt[8] = ???
  check(foo(x))
