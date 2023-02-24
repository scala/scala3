//> using options -language:experimental.inlineTraits
inline trait A(val x: Int)

class C extends A(10):
  def x = 1000 // error: Needs override marker
