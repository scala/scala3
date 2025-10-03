import language.experimental.captureChecking
class Foo:
  self: Foo^ =>
  var data: Int = 42
  def foo: Int = self.data  // ok

  def test(): Unit =
    val f: () -> Int = () => this.data  // error
    val g: () ->{this} Int = () => this.data  // ok
