//> using options -language:experimental.specializedTraits

inline trait Vec[T: Specialized](val x: T)

def main = 
    val v = new Vec[Int](10) {}
