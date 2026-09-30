//> using options -language:experimental.specializedTraits
// This shouldn't generate any boxing thanks to the specialized version of Numeric

inline def foo[T: {Specialized, Numeric2}](x: T): T = 
    val num = summon[Numeric2[T]]
    num.plus(x, num.fromInt(1)) 

inline trait A[T: {Numeric2, Specialized}]:
    def bar(x: T): T = foo(x)

class B extends A[Int]:
    def baz(x: Int): Int = foo(x)

inline trait Numeric2[T: Specialized]:
  def fromInt(x: Int): T
  def plus(x: T, y: T): T
  def times(x: T, y: T): T

implicit object IntIsIntegral extends Numeric2[Int]:
  override def fromInt(x: Int): Int = x
  override def plus(x: Int, y: Int): Int = x + y
  override def times(x: Int, y: Int): Int = x * y
