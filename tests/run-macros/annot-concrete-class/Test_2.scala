//> using options -experimental -Yno-experimental

trait AFoo:
    def foo: String

@implementAFoo
class Foo extends AFoo

@main def Test =
    val foo = new Foo
    println(foo.foo)
