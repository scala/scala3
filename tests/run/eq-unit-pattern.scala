// Matching `()` against a value whose static type is abstract must not be
// reduced to a constant test: the value can still be the unit value.
sealed abstract class Void
case object UnitRef extends Void
case class Caught[A](ref: A) extends Void

def toVoid[A](ref: A): Void = ref match
  case () => UnitRef
  case _ => Caught(ref)

def isUnit[A](x: A): Boolean = x == ()

@main def Test =
  assert(toVoid(()) == UnitRef, toVoid(()))
  assert(toVoid(1) == Caught(1))
  assert(toVoid("a") == Caught("a"))
  val any: Any = ()
  assert(toVoid(any) == UnitRef, toVoid(any))
  val ref: AnyRef = ().asInstanceOf[AnyRef]
  assert(toVoid(ref) == UnitRef, toVoid(ref))
  assert(isUnit(()))
  assert(isUnit(any))
  assert(!isUnit(1))
