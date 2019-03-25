trait BS[T, S <: BS[T, S]]
 trait IS extends BS[Int, IS]

sealed trait BSElem[T, S <: BS[_, S]]
  // old error: Type argument S does not conform to upper bound BS[Any, LazyRef(S)]

object BSElem {
  implicit val intStreamShape: BSElem[Int, IS] = ???
}
class Ops[A] {
  def asJavaSeqStream[S <: BS[_, S]](implicit s: BSElem[A, S]): S = ???
    // old error: Type argument S does not conform to upper bound BS[Any, LazyRef(S)]
}
