trait Syntax[F[_]] {
  def [A](a: A) ret: F[A]
}

trait Instance[A]

implicit val instanceSyntax: Syntax[Instance] = new Syntax[Instance] {
  def [A](a: A) ret: Instance[A] = new Instance[A] {}
}

object Instance {
  def defer[A](a: => A): Instance[A] = {
    ().ret
    new Instance[A] {}
  }
}
