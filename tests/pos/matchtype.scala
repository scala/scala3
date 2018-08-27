import typelevel._
object Test {
  type T[X] = X match {
    case String => Int
    case Int => String
  }

  type Len[X] <: Int = X match {
    case Unit => 0
    case x *: xs => S[Len[xs]]
  }

  type T2 = Len[(1, 2, 3)]
  erased val x: 3 = erasedValue[T2]

  type T1 = S[0]

  erased val x2: 1 = erasedValue[T1]

  rewrite def checkSub[T1, T2] =
    rewrite typelevel.erasedValue[T1] match {
      case _: T2 => // OK
      case _ => error("not a subtype T1/T2")
    }

  rewrite def checkSame[T1, T2] = {
    checkSub[T1, T2]
    checkSub[T2, T1]
  }

  checkSame[T2, S[S[S[0]]]]

  type Head[X <: Tuple] = X match {
    case (x1, _) => x1
  }

  checkSame[Head[(Int, String)], Int]

  type Concat[X <: Tuple, Y <: Tuple] <: Tuple = X match {
    case Unit => Y
    case x1 *: xs1 => x1 *: Concat[xs1, Y]
  }

  type Elem[X <: Tuple, N] = X match {
    case x *: xs =>
      N match {
        case 0 => x
        case S[n1] => Elem[xs, n1]
      }
  }

  type Elem1[X <: Tuple, N] = (X, N) match {
    case (x *: xs, 0) => x
    case (x *: xs, S[n1]) => Elem1[xs, n1]
  }

  erased val x3: String = erasedValue[Elem[(String, Int), 0]]
  erased val x4: Int = erasedValue[Elem1[(String, Int), 1]]

  checkSame[Elem[(String, Int, Boolean), 0], String]
  checkSame[Elem1[(String, Int, Boolean), 1], Int]
  checkSame[Elem[(String, Int, Boolean), 2], Boolean]

  checkSame[Concat[Unit, (String, Int)], (String, Int)]
  checkSame[Concat[(Boolean, Boolean), (String, Int)], Boolean *: Boolean *: (String, Int)]
  checkSub[(Boolean, Boolean, String, Int), Concat[(Boolean, Boolean), String *: Int *: Unit]]
}