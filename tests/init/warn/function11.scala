final class Capture {
  private var m: Boolean = false

  (0 to 10).foreach { i =>      // warn
    f()
  }

  val a = 10

  def f() = while ({
    println(a)
    m
  }) ()
}

final class Capture2 {
  private var m: Boolean = false

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