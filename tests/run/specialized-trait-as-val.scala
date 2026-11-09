//> using options -language:experimental.specializedTraits
inline trait Trait[T: Specialized]:
    def do_something() = println("Good morning")
  
inline trait A[T: Specialized]:
    val t = new Trait[T] {}

@main def Test = 
    val b = new A[Int]() {}
    b.t.do_something()
