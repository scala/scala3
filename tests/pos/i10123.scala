class D
class C[+T](x: T)

class Foo() {
  val status: Int = 0
}

object Main {
  implicit class RichC[T](c: C[T]) {
    def await(implicit d: D = ???): T = ???
  }

  def test1: Int = {
    val foo = new C(new Foo()).await
    foo.status
  }

  val test2 = new C(new Foo()).await.status
}