object Test1 {
  trait Foo[A]:
    def foo[C]: C => A

  // Works with old-style conversion
  implicit def i2f[A](a: A): Foo[A] = new Foo[A]:
    def foo[C]: C => A = _ => a

  // But not with newstyle
  /*
  given [A]: Conversion[A, Foo[A]] with
    def apply(a: A) = new Foo[A]:
      def foo[C]: C => A = _ => a
  */

  1.foo.foo
  1.foo.foo[String]
  1.foo[String].foo
  1.foo[String].foo[String]
}
object Test2 {

  // Works with extension method
  extension [A, C](a: A):
    def foo: C => A = _ => a

  1.foo.foo

  // ... but have to pass 2 parameters
  1.foo.foo[Any => Int, String]
  1.foo[Int, String].foo
  1.foo[Int, String].foo[String => Int, String]

}

