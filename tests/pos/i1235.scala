case class LazyList[T](headThunk: () => T, tailThunk: () => LazyList[T]){
  lazy val head = headThunk()
  lazy val tail = tailThunk()
}

object ~: {
  def unapply[T](x: LazyList[T]) = Some((x.head, x.tail))
}

object MinimizedMatchFail {
  val ll = LazyList(() => 1, () => LazyList(() => 2, () => ???))

  ll match {
    case lb ~: rest => println("success")
  }
}
