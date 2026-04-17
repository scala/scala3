inline trait Trait[T]:
    def do_something() = println("Good morning")
  
inline trait A[T]:
    def foo: Trait[T]

inline trait B extends A[Int]:
    override def foo = new Trait[Int] {}

@main def Test = 
    val b = new B() {}
    val f = b.foo
    f.do_something()
