//> using options -language:experimental.specializedTraits
//> using options -language:experimental.specializedTraits
inline trait T1[T: Specialized]
inline trait T2[T: Specialized] extends T1[?] // error: Wildcard types may not be substituted for Specialized type parameters.

inline trait T3[T: Specialized, E, F: Numeric]
inline trait T4 extends T3[?, ?, ?] // error: Wildcard types may not be substituted for Specialized type parameters.
