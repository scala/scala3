import language.experimental.captureChecking

class A:
  def f(x: A^): A^{this, x} = ???
