//> using options -language:experimental.specializedTraits

inline trait Vec[T: Specialized](var x: T)

def main = 
    val v = new Vec[Int](10) {}
