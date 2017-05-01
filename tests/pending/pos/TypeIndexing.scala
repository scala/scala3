// works in neither scalac nor dotty, but maybe could be made to work?
  trait A {
    type T
    val t : T
  }

  object A {
    def unapply(arg: A): Option[arg.T] = Some(arg.t)
  }

object Test {
  def use(a : A) = a match  {
    case b @ A(t) ⇒
      val s: b.T = t // type mismatch.
                     // found t.type (with underlying type <unapply-selector>.T)
                     // required: a.T
  }
}
