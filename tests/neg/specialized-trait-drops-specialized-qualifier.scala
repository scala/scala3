//> using options -language:experimental.specializedTraits
inline trait T1[T: Specialized]
inline trait T2[S] extends T1[S] // error: S must be Specialized as it substitutes for T: Specialized in inline trait T1
