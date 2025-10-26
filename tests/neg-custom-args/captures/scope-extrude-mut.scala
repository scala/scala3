import language.experimental.captureChecking

class A extends caps.Mutable:
  var x = 0

class B:
  private var a: A^ = A()
  def b() =
    val a1 = A()
    a = a1  // error