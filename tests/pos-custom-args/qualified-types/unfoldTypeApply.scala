// The result-type qualifier of a generic method must be picked up through the
// `TypeApply` node wrapping the applied method (see `ENode.resolvedInfo`).

def first[A](l: List[A] with l != Nil): A = l.head

def isNonEmpty[A](l: List[A]): {b: Boolean with b == (l != Nil)} = l != Nil

def test(l: List[Int]): Unit =
  if isNonEmpty(l) then
    // `isNonEmpty(l) == (l != Nil)` (from the result qualifier, resolved through
    // the type application) plus the branch condition `isNonEmpty(l) == true`
    // give `l != Nil`.
    first(l)
