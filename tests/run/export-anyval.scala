class Foo(val x: String) 


class Bar(val y: Foo) extends AnyVal:
    export y.*
    def foo: String = x
end Bar

@main def Test = 
    val a = Bar(Foo("Hello from export"))
    println(a.foo)

