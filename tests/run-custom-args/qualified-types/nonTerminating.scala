// The qualifier `asNonEmpty(l)` references a non-terminating predicate. The
// result-type qualifier is resolved through the `TypeApply` node (see
// `ENode.resolvedInfo`), so this compiles. At runtime the qualifier check
// currently lets an empty list through, so `l.head` throws. The non-termination
// of the runtime check itself is a separate issue to be addressed later.

def first[A](l: List[A] with l != Nil): A = l.head

def asNonEmpty[A](l: List[A]): {b: Boolean with b == true && l != Nil} =
  val res = asNonEmpty(l)
  res

@main def Test =
  val l = Nil: {l: List[Int] with asNonEmpty(l)}
  try println(l.head)
  catch case _: NoSuchElementException => println("threw NoSuchElementException")
