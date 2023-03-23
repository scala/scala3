inline trait A:
  def x(foo: Int)(bar: Unit) =
    bar
    foo

  def y(foo: Int) = x(foo)

class B extends A:
  def f = x