import typelevel._
object Test {
  type T[X] = X match {
    case String => Int
    case Int => String
  }

  trait Nat {
    def toInt: Int = ???
  }

  case object Z extends Nat
  case class S[N <: Nat] extends Nat
  type Z = Z.type

  type Len[X] = X match {
    case Unit => Z
    case x *: xs => S[Len[xs]]
  }

  type T2 = Len[(1, 2, 3)]
  erased val x: S[S[S[Z]]] = erasedValue[T2]

  rewrite def checkSub[T1, T2] =
    rewrite typelevel.erasedValue[T1] match {
      case _: T2 => // OK
      case _ => error("not a subtype T1/T2")
    }

  rewrite def checkSame[T1, T2] = {
    checkSub[T1, T2]
    checkSub[T2, T1]
  }

  checkSame[T2, S[S[S[Z]]]]

  type Head[X <: Tuple] = X match {
    case (x1, _) => x1
  }

  checkSame[Head[(Int, String)], Int]

  type Concat[X <: Tuple, Y <: Tuple] = X match {
    case Unit => Y
    case x1 *: xs1 => x1 *: Concat[xs1, Y]
  }

  checkSame[Concat[Unit, (String, Int)], (String, Int)]
  checkSame[Concat[(Boolean, Boolean), (String, Int)], Boolean *: Boolean *: (String, Int)]
  checkSub[(Boolean, Boolean, String, Int), Concat[(Boolean, Boolean), String *: Int *: Unit]]
}