object i7044 {
  case class Seg[T](pat:Pat[T], body:T)

  trait Pat[T]
  object Pat {
    case class Expr()            extends Pat[Int]
    case class Opt[S](el:Pat[S]) extends Pat[Option[S]]
  }

  def test[T](s:Seg[T]):Int = s match {
    case Seg(Pat.Expr(),body)          => body + 1
    case Seg(Pat.Opt(Pat.Expr()),body) => body.get
  }
}
