import scala.deriving.Mirror

val mT23 = summon[Mirror.Of[(
  Int *: Int *: Int *: Int *: Int *: Int *: Int *: Int *: Int
    *: Int *: Int *: Int *: Int *: Int *: Int *: Int *: Int *: Int
    *: Int *: Int *: Int *: Int *: Int *: EmptyTuple)]] // error
