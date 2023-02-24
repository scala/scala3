//> using options -language:experimental.inlineTraits
inline trait A:
  def f = (i: Int) => i

class B extends A:
  def g = f