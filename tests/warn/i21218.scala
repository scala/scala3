def Test[U, A](thisElem: A, thatElem: U) = {
  case object passedEnd
  val any: Seq[Any] = ???
  any.zip(any)
    .map {
      case (`passedEnd`, r: U @unchecked) => (thisElem, r)
      case (l: A @unchecked, `passedEnd`) => (l, thatElem)
      case t: (A, U) @unchecked           => t // false-positive warning
    }
}
