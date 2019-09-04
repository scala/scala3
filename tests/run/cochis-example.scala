
import Predef.{$conforms => _}
trait A {
  given id[X] as (X => X) = x => x
  def trans[X](x: X) given (f: X => X) = f(x)  // (2)
}
object Test extends A with App{
  given succ as (Int => Int) = x => x + 1     // (3)
  def bad[X](x: X): X = trans[X](x)              // (4) unstable definition !
  val v1 = bad [Int] (3)                         // (5) evaluates to 3
  assert(v1 == 3)
  val v2 = trans [Int] (3)
  assert(v2 == 4)
}