import scala.annotation.unchecked.uncheckedVariance as uV

trait Test:
  def split[I, M](in: Flow[I, Byte, M]): SubFlow[Byte, M, in.Repr]
  def test =
    split(new Flow[Int, Byte, Unit])
      .via[Char]
      .merge
      .filter()

trait FlowOps[+Out, +Mat]:
  type Repr[+O] <: FlowOps[O, Mat] { type Repr[+O] = FlowOps.this.Repr[O] }
  def via[O]:   Repr[O]   = ???
  def filter(): Repr[Out] = ???

class Flow[-In, +Out, +Mat] extends FlowOps[Out, Mat]:
  type Repr[+O] = Flow[In @uV, O, Mat @uV]

class SubFlow[+Out, +Mat, +F[+_]] extends FlowOps[Out, Mat]:
  type Repr[+O] = SubFlow[O, Mat @uV, F @uV]
  def merge: F[Out] = ???
