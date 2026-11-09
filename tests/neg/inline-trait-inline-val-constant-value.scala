//> using options -language:experimental.inlineTraits
inline trait A[T](x: T):
  inline val property = x // error: inline value must have a literal constant type
