trait Syntax[F[_]] {
  def (a: A) ret[A]: F[A]
}

trait Instance[A]

implicit val instanceSyntax: Syntax[Instance] = new Syntax[Instance] {
  def (a: A) ret[A]: Instance[A] = new Instance[A] {}
}

object Instance {
  def defer[A](a: => A): Instance[A] = {
    ().ret
    new Instance[A] {}
  }
}
