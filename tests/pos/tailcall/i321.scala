class i321[T >: Null <: AnyRef] {

  def mapconserve(f: T => Int): Int = {
    def loop(pending: T): Int = {
        val head1 = f(pending)
        loop(pending)
      }
    loop(null)
  }
}