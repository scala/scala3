package enums

object test1 {

  enum E4 {
    case C1(x: INT) // error: illegal reference
    case C2(x: Int = defaultX) // error: illegal reference
    case C3[T <: INT]() // error: illegal reference
  }

  object E4 {
    type INT = Integer
    val defaultX = 2
  }
}

object test2 {
  import E5.*
  object E5 {
    type INT = Integer
    val defaultX = 2
  }

  enum E5 {
    case C1(x: INT) // ok
    case C2(x: Int = defaultX) // ok
    case C3[T <: INT]() // ok
  }
}

object test3 {
  object E5 {
    type INT = Integer
    val defaultX = 2
  }

  import E5.*

  enum E5 {
    case C1(x: INT) // ok
    case C2(x: Int = defaultX)// ok
    case C3[T <: INT]() // ok
  }
}

object test4 {

  enum E5 {
    case C1(x: INT) // error: illegal reference
    case C2(x: Int = defaultX) // error: illegal reference
    case C3[T <: INT]() // error: illegal reference
  }

  import E5.*

  object E5 {
    type INT = Integer
    val defaultX = 2
  }
}

object test5 {
  enum E5[T](x: T) {
    case C3() extends E5[INT](defaultX)// error: illegal reference  // error: illegal reference
    case C4 extends E5[INT](defaultX) // error: illegal reference  // error: illegal reference
    case C5 extends E5[E5[_]](E5.this) // error: type mismatch
  }

  object E5 {
    type INT = Integer
    val defaultX = 2
  }
}

object test6 {
  import E5.*
  enum E5[T](x: T) {
    case C3() extends E5[INT](defaultX) // ok
    case C4() extends E5[INT](defaultX) // ok
  }

  object E5 {
    type INT = Integer
    val defaultX = 2
  }
}

object test7 {

  trait Arg

  enum E(x: Arg) {
    case C() extends E(this) // error: illegal reference to `this`
  }
  object E extends Arg
}
