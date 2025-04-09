import language.experimental.captureChecking

class B extends A:
  def f[cap C](x: AnyRef^{C}): Unit = ???
