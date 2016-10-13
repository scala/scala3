case class W[T](seq: Option[Option[T]] = Option.empty)
object W {
  def apply[T] = new W[T]()
}

case class V[T](vv: W[W[T]] = W.apply)
object Test {
  W[Int]()
  // V[Int]()    fails in scalac and dotty: both instantiate the vv-default to W[Nothing]
}
