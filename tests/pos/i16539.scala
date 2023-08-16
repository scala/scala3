//> using options -Werror
sealed trait Tag[A]

enum Hidden:
  case Reveal[A](
    init: A,
    reduce: (A, A) => A
  ) extends Hidden with Tag[A]

trait Handle[C]:
  def apply[A](c: C & Tag[A]): A

val x = new Handle[Hidden] {
  def apply[A](c: Hidden & Tag[A]): A = c match {
    case Hidden.Reveal(x, _) => x
  }
}
