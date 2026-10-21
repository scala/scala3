//> using options -language:experimental.specializedTraits

inline trait A[T: Specialized, D: Specialized]:
  def foo(x: T): T = x
  def bar(x: D): D = x
inline trait B[S: Specialized] extends A[S, Int]
class C extends B[Char]
