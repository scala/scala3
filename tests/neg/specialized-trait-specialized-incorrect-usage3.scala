//> using options -language:experimental.specializedTraits
//> using options -language:experimental.specializedTraits
trait Illegal:
    def x(v: Specialized) = 10 // error: Missing type parameter for Specialized
