//> using options -language:experimental.specializedTraits

object MySpecializedStuff:
    inline trait Foo[T: Specialized]:
        def bar = "Bar"

object MySpecializedStuff2:
    class Bar extends Foo[Char]
    def foo(x: Foo[Char]) = x.bar

@main def Test = 
    val x = new MySpecializedStuff2.Bar()
    assert(MySpecializedStuff2.foo(x) == "Bar")
