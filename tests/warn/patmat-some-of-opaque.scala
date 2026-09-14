object O:
  opaque type X = Int

object T:
  def f(o: Option[O.X]) = o match
    case Some(x: O.X) => 1 // OK, we're not actually type-testing x, it has to be O.X
    case None => 0

  def f2(o: Option[O.X]) = o match
    case None => 0
    case Some(x: O.X) => 1 // ditto in reverse order

