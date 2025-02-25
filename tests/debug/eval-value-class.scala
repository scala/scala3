trait A extends Any:
  def take(size: Size): A

class B(val x: String) extends AnyVal with A:
  def take(size: Size): B =
    new B(x.take(size.value))

  def +(b: B): B =
    new B(x + b.x)

  override def toString: String = s"B($x)"

class Size(val value: Int)

class Msg(size: Size):
  override def toString = "Hello, World!".take(size.value)

object Test:
  var b1 = new B("foo")
  private var size = new Size(2)

  def main(args: Array[String]): Unit =
    val b2 = bar
    println(b1.take(size))
    println(m(b1 + b2))

  def m(a: A): A =
    val size = new Size(5)
    a.take(size)

  def bar: B = new B("bar")
