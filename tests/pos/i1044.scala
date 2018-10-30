object Test {
  implicit class NN[T](x: T|Null) {
    def nn: T = x.asInstanceOf[T]
  }

  val x = ???.getClass.getMethods.nn.head.nn.getParameterTypes.nn.mkString(",")
}
