//> using options -language:experimental.specializedTraits
inline trait A[T: Specialized](parameter: Int):
    val x = parameter

def main = 
    val y = new A[Int](parameter = 1000) {}
