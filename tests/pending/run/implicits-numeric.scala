object Test extends App {

  implicit def _1: Long = 1L
  implicit def _2: Int = 0

  println(implicitly[AnyVal])
}
