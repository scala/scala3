//> using options -language:experimental.inlineTraits
trait One
trait Two extends One

inline trait A:
  type T <: One

class C extends A:
  type T = Two
