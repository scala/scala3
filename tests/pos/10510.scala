sealed trait Bool
case object True extends Bool
case object False extends Bool

sealed trait SBool[B <: Bool]
case object STrue extends SBool[True.type]
case object SFalse extends SBool[False.type]

type Not[B <: Bool] <: Bool = B match {
  case True.type => False.type
  case False.type => True.type
}

def not[B <: Bool](b: SBool[B]): SBool[Not[B]] = b match {
  case STrue => SFalse
  case SFalse => STrue
}
