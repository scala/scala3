class Foo {
  implicit class NN[T](x:T|Null) {
    def nn: T = x.asInstanceOf[T]
  }

  val x1: String|Null = null
  x1.nn.length
  val x2: String = x1.nn
  x1.nn.length
}
