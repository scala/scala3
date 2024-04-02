//> using options -language:experimental.modularity -source future
import compiletime.deferred

trait Ord[Self]:
  def less(x: Self, y: Self): Boolean

trait A:
  type Elem
  given Ord[Elem] = deferred
  def foo = summon[Ord[Elem]]

object Inst:
  given Ord[Int]:
    def less(x: Int, y: Int) = x < y

object Test:
  import Inst.given
  class C extends A:
    type Elem = Int
  object E extends A:
    type Elem = Int
  given A:
    type Elem = Int

class D[T: Ord] extends A:
  type Elem = T




