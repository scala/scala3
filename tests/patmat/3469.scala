object O {
  sealed trait Trait[+A] { type T }
  case class CaseClass[+A](a: A) extends Trait[A] { type T = Nothing }

  def id[TT, A](v: Trait[A] { type T = TT }): Trait[A] { type T = TT } =
    v match {
      case CaseClass(a) => CaseClass(a)
    }
}