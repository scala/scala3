trait Syntax[F[_]] {
  extension [A](a: A) def ret: F[A]
}

trait Instance[A]

implicit val instanceSyntax: Syntax[Instance] = new Syntax[Instance] {
  extension [A](a: A) def ret: Instance[A] = new Instance[A] {}
}

object Instance {
  def defer[A](a: => A): Instance[A] = {
    ().ret
    new Instance[A] {}
  }
}
