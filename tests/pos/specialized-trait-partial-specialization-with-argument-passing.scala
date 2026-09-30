//> using options -language:experimental.specializedTraits

inline trait Foo[T: Specialized, S: Specialized] 
inline trait Bar[T: Specialized] extends Foo[T, Int]
class Baz extends Bar[Boolean] 

inline def receiver[S: Specialized](x: Foo[S, Int]) = 
    println("Good Morning")

val x = receiver(Baz())
