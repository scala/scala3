type Validated[A] = A
class Foo:
  def >(x: Int) = true
def foo(v: Foo|Validated[Foo]): Unit = v > 10
