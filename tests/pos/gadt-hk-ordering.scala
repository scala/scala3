object test {

  enum KSUB[-F[_], +G[_]] {
    case Refl[S[_]]() extends KSUB[S, S]
  }

  def foo[F[_]](ksub: Option KSUB F) =
    ksub match {
      case KSUB.Refl() =>
        // we have (s is type parameter of KSUB.Refl):
        // f >: Option
        // s <: f
        val fi: F[Int] = Option(0)
        ()
    }
}
