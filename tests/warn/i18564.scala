
//> using options -Wunused:imports

import scala.compiletime.*
import scala.deriving.*

trait Foo

trait HasFoo[A]:
  /** true if A contains a Foo */
  val hasFoo: Boolean

// given instances that need to be imported to be in scope
object HasFooInstances:
  given defaultHasFoo[A]: HasFoo[A] with
    val hasFoo: Boolean = false
  given HasFoo[Foo] with
    val hasFoo: Boolean = true

object HasFooDeriving:

  inline private def tupleHasFoo[T <: Tuple]: Boolean =
    inline erasedValue[T] match
    case _: EmptyTuple => false
    case _: (t *: ts)  => summonInline[HasFoo[t]].hasFoo || tupleHasFoo[ts]

  inline def deriveHasFoo[T](using p: Mirror.ProductOf[T]): HasFoo[T] =
    // falsely reported as unused even though it has influence on this code
    import HasFooInstances.given // no warn at inline method
    val pHasFoo = tupleHasFoo[p.MirroredElemTypes]
    new HasFoo[T]: // warn New anonymous class definition will be duplicated at each inline site
      val hasFoo: Boolean = pHasFoo

/* the import is used upon inline elaboration
object Test:
  import HasFooDeriving.*
  case class C(x: Foo, y: Int)
  def f: HasFoo[C] = deriveHasFoo[C]
*/
