//> using options -language:experimental.specializedTraits

inline trait A[T: Specialized](val x: T):
    opaque type Special = Int
