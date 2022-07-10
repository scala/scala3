import scala.deriving.Mirror

val mT23 = summon[Mirror.Of[( // error
  Int *: Int *: Int *: Int *: Int *: Int *: Int *: Int *: Int
    *: Int *: Int *: Int *: Int *: Int *: Int *: Int *: Int *: Int
    *: Int *: Int *: Int *: Int *: Int *: EmptyTuple)]]
