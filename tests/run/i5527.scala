trait Contravariant[F[_]] {
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
}

object Library {

  opaque type Set[A] = A => Boolean

  object Set {
    def singleton[A](a: A): Set[A] =
      _ == a
  }

  implicit class SetOps[A](private val set: Set[A]) extends AnyVal {
    def contains(a: A): Boolean = set(a)
  }

  implicit val setContravariant: Contravariant[Set] = new Contravariant[Set] {
    def contramap[A, B](fa: Set[A])(f: B => A): Set[B] =
      b => fa(f(b))
  }
}

object Test extends App {
  import Library._
  //import Library.Set.setContravariant if this is imported the program will run correctly

  val F = implicitly[Contravariant[Set]]

  val one = Set.singleton(1)
  val char = F.contramap(one)((s: String) => s.length)
  assert(char.contains("a"))
  assert(!char.contains("ab"))
}