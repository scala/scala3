class Future[T]
object Future:
  class Collector[T](fs: (Future[T]^)*)
  class MutableCollector[T](val futures: (Future[T]^)*) extends Collector[T](futures*):
    def add(future: Future[T]^{futures*}) = ???
