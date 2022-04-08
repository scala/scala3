class Foo(val id: Int) {
    inline def ==(that: Foo): Boolean = true
}
case class FooWrapper(foo: Foo)
