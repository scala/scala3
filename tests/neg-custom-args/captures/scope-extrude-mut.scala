import language.experimental.captureChecking

class A extends caps.Stateful:
  var x = 0

class B extends caps.Stateful:
  private var a: A^ = A()
  def b() =
    val a1 = A()
    a = a1  // error