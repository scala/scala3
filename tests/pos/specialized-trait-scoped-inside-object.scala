//> using options -language:experimental.specializedTraits

object MySpecializedStuff:
    inline trait Foo[T: Specialized]:
        def bar = "Bar"
    
    def foo = new Foo[Int] {}

@main def main = MySpecializedStuff.foo.bar
