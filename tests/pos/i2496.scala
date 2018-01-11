sealed trait F { type A }
case object FI extends F { type A = Int }
case object FS extends F { type A = String }
