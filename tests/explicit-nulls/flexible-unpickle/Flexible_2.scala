import unsafeNulls.Foo.*
import unsafeNulls.Unsafe_1

class Inherit_1 extends Unsafe_1 {
  override def foo(s: String): String = s
  override def bar[T >: String](s: T): T = s
  override def bar2[T >: String | Null](s: T): T = s
  override def bar3[T <: Function1[String,String]](g: T) = g
  override def bar4[HK[_]](i: String | Null): HK[String | Null] = ???
}

class Inherit_2 extends Unsafe_1 {
  override def foo(s: String | Null): String | Null = null
  override def bar[T >: String](s: T | Null): T | Null = s
  override def bar2[T >: String](s: T): T = s
  override def bar3[T <: Function1[(String|Null),(String|Null)]](g: T) = g
  override def bar4[HK[_]](i: String): HK[String] = ???
}

class Inherit_3 extends Unsafe_1 {
  override def foo(s: String): String | Null = null
  override def bar[T >: String](s: T): T | Null = s
}

class Inherit_4 extends Unsafe_1 {
  override def foo(s: String | Null): String = "non-null string"
  override def bar[T >: String](s: T | Null): T = "non-null string"
}

case class cc()

@main
def Flexible_2() =
  val s2: String | Null = "foo"
  val unsafe = new Unsafe_1()
  val s: String = unsafe.foo(s2)
  unsafe.foo("")
  unsafe.foo(null)

  val refinement = new Unsafe_1 {val b: String}