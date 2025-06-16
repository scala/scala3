package unsafeNulls

class Unsafe_1 {
  def foo(s: String): String = {
    if (s == null) then "nullString"
    else s
  }
  def bar[T >: String](s: T): T = {
    ???
  }
  def bar2[T >: String | Null](s: T): T = {
    ???
  }
  def bar3[T <: Int => Int](g: T): T = g
}

object Foo {
  def bar = "bar!"
}