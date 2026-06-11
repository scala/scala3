//> using options -language:experimental.specializedTraits
trait Trait[T: Specialized] // error: Only inline traits and inline functions may take Specialized type parameters
def t[T: Specialized](a: T) = "Output" // error: Only inline traits and inline functions may take Specialized type parameters
