//> using options -language:experimental.specializedTraits

object MySpecializedStuff:
    inline trait Foo[T: Specialized]:
        def bar = "Bar"
    
    class Bar extends Foo[Char]
    def foo(x: Foo[Char]) = x.bar

@main def Test = 
    val x = new MySpecializedStuff.Bar()
    assert(MySpecializedStuff.foo(x) == "Bar")
