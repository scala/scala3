//> using options -language:experimental.specializedTraits
inline trait Trait[T: Specialized]:
    def do_something() = println("Good morning")
  
inline trait A[T: Specialized]:
    def foo: Trait[T]

inline trait B[S: Specialized] extends A[S]:
    override def foo = new Trait[S] {}

@main def Test = 
    val b = new B[Int]() {}
    val f = b.foo
    f.do_something()
