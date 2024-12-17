//> using options -experimental

import scala.annotation.unroll

@unroll
class UnrollClass // error

@unroll
trait UnrollTrait // error

@unroll
object UnrollObject // error

@unroll
type UnrollType = Int // error

@unroll
enum UnrollEnum { case X } // error

object wrap {
  val annotExpr: Int = 23: @unroll // error
  type annotType = Int @unroll // error

  @unroll
  val unrollVal: Int = 23 // error

  @unroll
  def unrollDef: Int = 23 // error
}
