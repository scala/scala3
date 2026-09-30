//> using options -language:experimental.specializedTraits
inline trait Trait[T: Specialized]:
    def do_something() = println("Good morning")
  
inline trait A[T: Specialized]:
    def foo: Trait[T]

inline trait B extends A[Int]:
    override def foo = new Trait[Int] {}

@main def Test = 
    val b = new B() {}
    val f = b.foo
    f.do_something()
