// See also neg/i1181c.scala for a variant which doe not compile
class Foo[A]

trait Bar[DD[_,_]] {
  val x: DD[Int, Int]
}

object Test {
  type F[X, Y] = Foo[X]

  type LAMBDA[X,Y] = Foo[X]
  trait Baz extends Bar[LAMBDA] {
    def foo[M[_,_]](x: M[Int, Int]) = x

    foo(x) // error: found: Foo[Int](Baz.this.x) required: M[Int, Int]
  }
}
