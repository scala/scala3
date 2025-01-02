
//> using options -Wunused:imports

import scala.compiletime.ops.int.* // no warn

object TupleOps:
  /** Type of the element at position N in the tuple X */
  type Elem[X <: Tuple, N <: Int] = X match {
    case x *: xs =>
      N match {
        case 0 => x
        case S[n1] => Elem[xs, n1]
      }
  }

  /** Literal constant Int size of a tuple */
  type Size[X <: Tuple] <: Int = X match {
    case EmptyTuple => 0
    case x *: xs => S[Size[xs]]
  }

object Summoner:
  transparent inline def summoner[T](using x: T): x.type = x

object `Summoner's Tale`:
  import compiletime.summonFrom // no warn
  inline def valueOf[T]: T = summonFrom: // implicit match
    case ev: ValueOf[T] => ev.value
  import Summoner.* // no warn
  def f[T](using T): T = summoner[T] // Inlined

class C:
  private def m: Int = 42 // no warn
object C:
  class D:
    private val c: C = C() // no warn
    export c.m // no work to do, expanded member is non-private and uses the select expr

object UsefulTypes:
  trait T
object TypeUser:
  import UsefulTypes.*
  def f(x: => T) = x
