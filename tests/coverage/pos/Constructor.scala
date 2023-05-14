package covtest

class C:
  def this(arg: String) = {
    this()
    g()
  }

  def this(x: Int) =
    this(x.toString() + "foo")

  def f(x: Int) = x
  def x = 1
  f(x)

  def g(): Int = 2

object O:
  def g(y: Int) = y
  def y = 1
  g(y)
