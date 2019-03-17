class C[+X] {
  type Id = X match {   // error: covariant type X appears in invariant position
    case Int => Int
    case String => String
  }

  type T1[Y] = Y match {  // error: covariant type X appears in invariant position
    case List[X] => Int
    case _ => String
  }
  type T2[Y] = Y match { // error: covariant type X appears in invariant position
    case List[_ >: X] => Int
    case _ => String
  }
  type T3[Y] = Y match { // error: covariant type X appears in invariant position
    case List[_ <: X] => Int
    case _ => String
  }

  def foo[Y <: X](): Unit = ??? // error: covariant type X appears in contravariant position
  type Foo[Y <: X] <: Y  // error: covariant type X appears in contravariant position
}
class D[-X] {

  type Id2 = X match { // error: contravariant type X appears in invariant position
    case Int => Int
    case String => String
  }
  type T4[Y] = Y match {  // error: contravariant type X appears in invariant position
    case List[X] => Int
    case _ => String
  }
  type T5[Y] = Y match { // error: contravariant type X appears in invariant position
    case List[_ >: X] => Int
    case _ => String
  }
  type T6[Y] = Y match { // error: contravariant type X appears in invariant position
    case List[_ <: X] => Int
    case _ => String
  }
  type T7[Y] = Y match { // error: contravariant type X appears in covariant position
    case List[_] => X
    case _ => String
  }
}