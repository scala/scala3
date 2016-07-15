object HKGADT {
  sealed trait Foo[F[_]]
  final case class Bar() extends Foo[List]

  def frob[F[_]](foo: Foo[F]) =
    foo match {
      case Bar() => ()
    }
}
