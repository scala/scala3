//> using options -language:experimental.specializedTraits
inline trait Trait[T: Specialized](x: Any):
    def do_something() = println("Good morning")

trait Trait2

@main def Test = 
    val b = new Trait(new Trait2() {}) {}
