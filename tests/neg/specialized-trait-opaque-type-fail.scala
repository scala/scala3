//> using options -language:experimental.specializedTraits
//> using options -language:experimental.specializedTraits

inline trait A[T: Specialized](val x: T):
    opaque type Special = T
    type Ordinary = T
    opaque type Special2 = Int
    
    def foo1: Special = 10  // error: 10 does not conform to T 
    def bar1: Ordinary = 10 // error: 10 does not conform to T
    def baz1: Special2 = 10 // This one is fine

class B extends A[Int](100):
    def foo2: Special = 10  // error: 10 does not conform to Special (same behaviour as ordinary traits)
    def bar2: Ordinary = 10 // This one is fine
    def baz2: Special2 = 10 // error: 10 does not conform to Special (same behaviour as ordinary traits) 
 