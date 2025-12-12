// Example in docs/_docs/reference/metaprogramming/inline.md

inline def double(inline x: Int): Int = x * 2
inline def eight: Int =
  inline val res = double(4)
  res
