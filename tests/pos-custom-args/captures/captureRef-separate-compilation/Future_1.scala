class Future[T]
object Future:
  class Collector[T, c^](fs: (Future[T]^{c})*)
  class MutableCollector[T, c^](val futures: (Future[T]^{c})*) extends Collector[T, {c}](futures*):
    def add(future: Future[T]^{c}) = ???
