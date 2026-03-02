import language.experimental.captureChecking

class B extends A:
  def f[C^](x: AnyRef^{C}): Unit = ???
