final class Capture {
  private[this] var m: Boolean = false

  (0 to 10).foreach { i =>      // error
    f()
  }

  val a = 10

  def f() = while ({
    println(a)
    m
  }) ()
}

final class Capture2 {
  private[this] var m: Boolean = false

  (0 to 10).foreach { i =>
    f()
  }

  val a = 10

  def f() = while ({
    m = false
    m
  }) ()

  (0 to 10).foreach { i =>
    f()
  }
}