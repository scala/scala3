trait Exec[T <: Exec[T]]

object Tree {
  sealed trait Next[+T, +PL, +P, +H, +A]

  sealed trait Child[+T, +PL, +P, +H, +A]

  sealed trait Branch[T <: Exec[T], PL, P, H, A] extends Child[T, PL, P, H, A] with NonEmpty[T, PL, P, H]

  sealed trait NonEmpty[T <: Exec[T], PL, P, H]

  case object Empty extends Next[Nothing, Nothing, Nothing, Nothing, Nothing]

  sealed trait RightBranch[T <: Exec[T], PL, P, H, A] extends Next[T, PL, P, H, A] with Branch[T, PL, P, H, A]

  trait BranchImpl[T <: Exec[T], PL, P, H, A] {
    def next: Next[T, PL, P, H, A]

    def nextOption: Option[Branch[T, PL, P, H, A]] =
      next match {  // crashes
        case b: RightBranch[T, PL, P, H, A] => Some(b)
        case Empty                          => None
      }
  }
}