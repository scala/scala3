class X(val underlying: Object) extends AnyVal

trait Producer[T] {
  def produce: T
}

class XProducer extends Producer[X] {
  def produce = new X(null) // error
}
