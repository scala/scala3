object Test extends App {
  object O
  implicit def foo(x: O.type): String = "hello"
  val s: String = O
  implicit def bar(x: s.type): Int = s.length
  //implicit def bar2(x: String): Int = s.length
  val l: Int = s
  assert(s == "hello")
  assert(l == 5)
}

object Test3781 {
  class Foo[T](val value : T)
  object Foo {
    implicit def fromXInt[T <: Int with Singleton](i : T): Foo[T] = new Foo[T](i)
  }
  class FooUser[T] {
    def op[T2](that : Foo[T2]) : FooUser[T2] = new FooUser[T2]
  }
  val f = new FooUser[1]
  val f2 = f op 2
}