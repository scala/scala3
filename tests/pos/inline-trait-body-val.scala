//> using options -language:experimental.inlineTraits
inline trait A:
  val x = 1

class B extends A:
  def f = x
