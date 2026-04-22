//> using options -language:experimental.specializedTraits
inline trait T1[T: Specialized]
inline trait T2[S] extends T1[S] // error: type S used in a Specialized position, so it must be marked as Specialized at its definition.

inline def foo[E](x: T1[E]) = 100 // error: type E used in a Specialized position, so it must be marked as Specialized at its definition. 

inline def foo[F] = new T1[F]() {} // error: type F used in a Specialized position, so it must be marked as Specialized at its definition. 
