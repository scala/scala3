
object Test {
  def main(args: Array[String]): Unit = {
    val xProducer = new XProducer
    println(xProducer.produce.underlying)

  }
}

class X(val underlying: Object) extends AnyVal

trait Producer[T] {
  def produce: T
}

class XProducer extends Producer[X] {
  def produce = new X(null)
}
