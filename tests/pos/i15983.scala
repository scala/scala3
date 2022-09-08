package perspective.examples

trait crash[ElemLabels <: Tuple] {
  type Index        = crash.IntIdx
  type Gen[A[_]]    = IArray[A[Any]]

  type Const[A] = [B] =>> A

  def tabulateFoldLeft[B](start: B)(f: (B, Index) => B): B = ???

  type TupleUnionLub[T <: Tuple, Lub, Acc <: Lub] <: Lub = T match {
    case (h & Lub) *: t => TupleUnionLub[t, Lub, Acc | h]
    case EmptyTuple     => Acc
  }

  type Names = TupleUnionLub[ElemLabels, String, Nothing]

  def indexK[A[_]](fa: Gen[A])(rep: Index): A[rep.X] =
    fa(rep.value).asInstanceOf[A[rep.X]]

  def nameToIndex[Name <: Names](name: Name): Unit =
    val n: Gen[Const[Names]] = ???
    val acc: List[(Names, Index)] = ???
    val res = tabulateFoldLeft(Nil: List[(Names, Index)]) { (acc, idx) =>
      (indexK(n)(idx), idx) :: acc
    }
}
object crash {
  class IntIdx(val value: Int) extends AnyVal {
    type X
  }
}