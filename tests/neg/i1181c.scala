// This program compiles with Dotty using refined types for application, but
// does not compile with native applications. The reason is that in previous
// Dotty the parameter reference to the lambda [X, Y] =>> Foo[X, Y] was a TypeRef
// which could be selected for partial application. But now the type lambda gets
// substituted directly, which prevents that conversion. The program compiles
// if the type lambda is replaced by a type alias (see pos/i1181c.scala).
class Foo[A]

trait Bar[DD[_,_]] {
  val x: DD[Int, Int]
}

object Test {
  type F[X, Y] = Foo[X]

  trait Baz extends Bar[[X,Y] =>> Foo[X]] {
    def foo[M[_,_]](x: M[Int, Int]) = x

    foo(x) // error: found: Foo[Int](Baz.this.x) required: M[Int, Int]
  }
}
