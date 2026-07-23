//> using options -language:experimental.specializedTraits
inline trait Trait[T: Specialized]:
    def do_something() = println("Good morning")
  
inline trait A[T: Specialized]:
    def foo(x: Trait[T]) = x 

@main def Test = 
    val b = new A[Long]() {}
    b.foo(new Trait[Long] {}).do_something()
