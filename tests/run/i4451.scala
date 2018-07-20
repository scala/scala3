class InitError extends Exception

class Lazy(x: Int) {
  private[this] var init = false
  lazy val value = {
    if (!init) {
      init = true
      throw new InitError
    }
    x
  }
}

class LazyVolatile(x: Int) {
  private[this] var init = false
  @volatile lazy val value = {
    if (!init) {
      init = true
      throw new InitError
    }
    x
  }
}

object Test {

  def tryTwice(x: => Int): Int =
    try {
      x; assert(false); 0
    } catch {
      case _: InitError => x
    }

  def lazyInMethod(x: Int) = {
    var init = false
    lazy val value = {
      if (!init) {
        init = true
        throw new InitError
      }
      x
    }
    tryTwice(value)
  }

  def main(args: Array[String]) = {
    val v = 42

    val l0 = new Lazy(v)
    val l1 = new LazyVolatile(v)
    assert(tryTwice(l0.value) == v)
    assert(tryTwice(l1.value) == v)
    assert(lazyInMethod(v) == v)
  }
}
