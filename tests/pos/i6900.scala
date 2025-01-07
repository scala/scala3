object Test1 {
  trait Foo[A]:
    def foo[C]: C => A

  // Works with old-style conversion
  implicit def i2f[A](a: A): Foo[A] = new Foo[A]:
    def foo[C]: C => A = _ => a

  // But not with newstyle
  /*
  given [A]: Conversion[A, Foo[A]]:
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
  extension [A](a: A)
    def foo[C]: C => A = _ => a

  1.foo.foo

  1.foo.foo[String]
  1.foo[String].foo
  1.foo[String].foo[String]

}

