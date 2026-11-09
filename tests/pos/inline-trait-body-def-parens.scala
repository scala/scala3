//> using options -language:experimental.inlineTraits
inline trait A:
  def x() = 1

class B extends A:
  def f = x()