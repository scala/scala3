//> using options -language:experimental.specializedTraits
//> using options -language:experimental.specializedTraits

// In principle we might be able to allow this because after erasure we will have different
// classes. The problem is we don't know the classes at the beginning of compilation
// so until erasure these overloads will be the same. It depends on to what extent the 
// pre-erasure phases rely on "pre-computing" post-erasure signatures. For now we block 
// it as it's not a requirement for specialized traits to work.

inline trait Foo[T: Specialized]:
    def foo(x: Foo[T]): Foo[T]

class Bar:
    def method1(x: Foo[Int]) = 10
    def method1(x: Foo[String]) = 10 // error: Conflicting definitions

@main def Test =
    val x = Bar()
    
