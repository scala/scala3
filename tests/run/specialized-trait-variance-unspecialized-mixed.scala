//> using options -language:experimental.specializedTraits

inline trait Fn[-T: Specialized, +R: Specialized]:
  def apply(x: T): R

object AnyToZero extends Fn[Any, Int]:
  def apply(x: Any) = 0
  
object Succ extends Fn[Int, Int]:
  def apply(x: Int) = x + 1

def call(f: Fn[Int, Int]): Int = f(41)

@main def Test =
  assert(call(AnyToZero) == 0)
  assert(call(Succ) == 42)
