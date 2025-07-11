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
  def bar3[T <: Function1[String,String]](g: T): T = g
  def bar4[HK[_]](i: String): HK[String] = ???
}

object Foo {
  def bar = "bar!"
  def id[T](t: T): T = t

}

class Constructors {

}