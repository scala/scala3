
import Predef.{assert, $conforms as _}
trait A {
  given id: [X] => (X => X) = x => x
  def trans[X](x: X)(using f: X => X) = f(x)  // (2)
}
object Test extends A with App{
  given succ: (Int => Int) = x => x + 1     // (3)
  def bad[X](x: X): X = trans[X](x)              // (4) unstable definition !
  val v1 = bad [Int] (3)                         // (5) evaluates to 3
  assert(v1 == 3)
  val v2 = trans [Int] (3)
  assert(v2 == 4)
}