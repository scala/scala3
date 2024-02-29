trait Show[-A >: Nothing]

type MT1[I <: Show[Nothing], N] = I match
  case Show[a] => N match
    case Int => a

val a = summon[MT1[Show[String], Int] =:= String]
