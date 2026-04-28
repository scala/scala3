//> using options -language:experimental.specializedTraits
def x: Specialized[Int] = new Specialized[Int] {} // error: Cannot extend sealed trait Specialized in a different source file
