class Cl[T] {

  sealed trait A {
    def foo = this match {
      case AObj => 0
      case BObj => 0
      case ACls(x) => 0
      case BCls(x) => 0
    }
  }

  case object AObj extends A
  case class ACls(x: Int) extends A

  sealed trait B extends A
  case object BObj extends B
  case class BCls(x: Int) extends B
}
