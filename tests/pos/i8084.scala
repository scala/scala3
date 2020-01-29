class Z
class O
object Test0 {
  def foo(x: Z | O) = ()
  def bar: Z | O = O()
  foo(bar)
}

object Test {
  def foo(x: 0 | 1) = ()
  def bar: 0 | 1 = 0
  foo(bar)
}

