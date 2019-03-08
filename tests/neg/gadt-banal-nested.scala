object banal {
  sealed trait T[A]
  final case class StrLit(v: String) extends T[String]
  final case class IntLit(v: Int) extends T[Int]

  def eval[A](t: T[A]): A = t match {
    case _: T[a] => t match {
      case StrLit(v) => v
      case IntLit(_) => "" // error
    }
  }
}
