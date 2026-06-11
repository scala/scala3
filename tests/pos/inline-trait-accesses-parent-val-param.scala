//> using options -Werror -Wsafe-init

inline trait A[T](val x: T):
  val v = x

inline trait B extends A[Int]:
  val z = x

class C extends B, A[Int](10)
