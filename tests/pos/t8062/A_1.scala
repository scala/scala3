package warmup

object Warmup {
  def filter[A](p: Any => Boolean): Int = 1 + filter[Any](p)
}
