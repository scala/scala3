inline trait Trait[T]:
    def do_something() = println("Good morning")

trait Trait2:
    def bar() = println("bar")

inline trait A[T]:
    def foo: Trait[T]

inline trait B extends A[Int]:
    override def foo = new Trait[Int] {
        val x = new Trait2() {}
    }

@main def Test = 
    val b = new B() {}
    val f = b.foo
    f.do_something()
