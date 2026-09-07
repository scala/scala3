//> using options -language:experimental.inlineTraits
inline trait A:
  def generate(x: Int) = x + 1

class B extends A:
  val y = generate(7)
