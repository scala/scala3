// https://github.com/lampepfl/dotty/issues/14096
object Test:
  object Forte:
    def test[T](i: Int, config: String = ""): Int = 1
    def test[T](i: String): Int = 2

  Forte.test[Int](1)
