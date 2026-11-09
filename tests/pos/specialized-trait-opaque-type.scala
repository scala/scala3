//> using options -language:experimental.specializedTraits

inline trait A[T](val x: T):
    opaque type Special = T

    def getSpecial: Special = x
    def eatSpecial(y: Special) = "Mmm, that was tasty!" 

class B extends A[Int](100)

def foo = 
    val b = B()
    println(b.eatSpecial(b.getSpecial))
