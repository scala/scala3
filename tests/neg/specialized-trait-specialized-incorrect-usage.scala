//> using options -language:experimental.specializedTraits
//> using options -language:experimental.specializedTraits
trait Illegal:
    def x: Int = 
        new Specialized[Int] {} // error: Cannot extend sealed trait Specialized in a different source file
        10

    class Baz extends Specialized[Int]  // error: Cannot extend sealed trait Specialized in a different source file

    trait A[T] extends Specialized[T]  // error: Cannot extend sealed trait Specialized in a different source file
