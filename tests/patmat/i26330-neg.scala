enum Foo { case Bar(); case Qux(); case Baz() }
case class Inv[A](a: A)

def missingCase(x: Inv[Foo]) = x match {
  case Inv[Foo.Bar @unchecked](_) => ()
  case Inv[Foo.Qux @unchecked](_) => ()
}

def wrongTypeArg(x: Inv[Int]) = x match {
  case Inv[String @unchecked](_) => ()
}

sealed trait X; case object X1 extends X; case object X2 extends X
sealed trait Y; case object Y1 extends Y; case object Y2 extends Y
case class Pair[A, B](a: A, b: B)

def diagonalMissing(p: Pair[X, Y]) = p match {
  case Pair[X1.type @unchecked, Y1.type @unchecked](_, _) => ()
  case Pair[X2.type @unchecked, Y2.type @unchecked](_, _) => ()
}
