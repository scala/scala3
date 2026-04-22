//> using options -language:experimental.specializedTraits
inline trait T1[T: Specialized]

inline trait T2 extends T1[List[Int]]                           // ok
inline trait T3[S] extends T1[List[S]]                          // error: S should be specialized
inline trait T4[S] extends T1[List[List[S]]]                    // error: S should be specialized
inline trait T5[S: Specialized] extends T1[List[S]]             // ok; should only specialize later
inline trait T6[T[_], S] extends T1[T[S]]                       // error: T should be specialized // error: S should be specialized
inline trait T7[T[_]] extends T1[T[Int]]                        // error: T should be specialized

inline def foo1[S](x: T1[List[S]]): Int = 10                    // error: S should be specialized
inline def foo2(x: T1[List[Int]]): Int = 10                     // ok
inline def foo3[S](x: T1[List[List[S]]]): Int = 10              // error: S should be specialized
inline def foo4[S: Specialized](x: T1[List[List[S]]]): Int = 10 // ok

inline def bar1[S] = new T1[List[S]]() {}                       // error: S should be specialized
inline def bar2 = new T1[List[Int]]() {}                        // ok
inline def bar3[S] = new T1[List[List[S]]]() {}                 // error: S should be specialized
inline def bar4[S: Specialized] = new T1[List[List[S]]]() {}    // ok

