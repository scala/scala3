//> using options -language:experimental.inlineTraits

inline trait A:
  def f(x: Int): Int = x
  def f(x: String): Int = f(0)

class B extends A:
  override def f(x: Int): Int = 1
  override def f(x: String): Int = 2
