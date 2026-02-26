//> using options -language:experimental.specializedTraits

inline trait MyInterface[T: Specialized]:
  def fromInt(x: Int): T
  def plus(x: T, y: T): T

implicit object Implementation extends MyInterface[Int]:
  override def fromInt(x: Int): Int = x
  override def plus(x: Int, y: Int): Int = x + y
