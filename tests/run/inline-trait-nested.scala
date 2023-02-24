//> using options -language:experimental.inlineTraits
// While we don't allow inner nested classes inside inline traits, we do allow creation of anonymous classes inside methods
// inside inline traits - after all these are just ordinary methods.

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
