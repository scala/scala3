trait Box[+T]
case class Foo[+S](s: S) extends Box[S]

def unwrap2[A](b: Box[A]): A =
  b match
  case _: Foo[Int] => 0 // warn

object Test1 {
  // Invariant case, OK
  sealed trait Bar[A]

  def test[A](bar: Bar[A]) =
    bar match {
      case _: Bar[Boolean] => ??? // warn
      case _ => ???
    }
}

object Test2 {
  // Covariant case
  sealed trait Bar[+A]

  def test[A](bar: Bar[A]) =
    bar match {
      case _: Bar[Boolean] => ??? // warn
      case _ => ???
    }
}

object Test3 {
  // Contravariant case
  sealed trait Bar[-A]

  def test[A](bar: Bar[A]) =
    bar match {
      case _: Bar[Boolean] => ??? // warn
      case _ => ???
    }
}
