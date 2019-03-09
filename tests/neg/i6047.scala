type Id[+X] = X match { // error: covariant type X appears in nonvariant position
  case Int => Int
  case String => String
}
type T1[+X, Y] = Y match {  // error: covariant type X appears in nonvariant position
  case List[X] => Int
  case _ => String
}
type T2[+X, Y] = Y match { // error: covariant type X appears in nonvariant position
  case List[_ >: X] => Int
  case _ => String
}
type T3[+X, Y] = Y match { // error: covariant type X appears in nonvariant position
  case List[_ <: X] => Int
  case _ => String
}
type Id2[-X] = X match { // error: contravariant type X appears in nonvariant position
  case Int => Int
  case String => String
}
type T4[+X, Y] = Y match {  // error: contravariant type X appears in nonvariant position
  case List[X] => Int
  case _ => String
}
type T5[+X, Y] = Y match { // error: contravariant type X appears in nonvariant position
  case List[_ >: X] => Int
  case _ => String
}
type T6[+X, Y] = Y match { // error: contravariant type X appears in nonvariant position
  case List[_ <: X] => Int
  case _ => String
}
type T7[-X, Y] = Y match { // error: contravariant type X appears in covariant position
  case List[_] => X
  case _ => String
}

class C[+X] {
  type Id = X match {
    case Int => Int
    case String => String
  }
}