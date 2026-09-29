//> using options -language:experimental.inlineTraits
inline trait A:
  private val x: Int = 1 // error: inline traits cannot have non-local private members
  def eq(o: A) = o.x == x
