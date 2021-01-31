// AE-77a004382f0de9965cb3880b27f302eb1c885c48
object Foo {
    class A
    def f(x: A): Unit = x { } // This error is necessary.
}
case class Foo()
type Foo = Any
