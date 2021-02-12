import scala.scalajs.js
import scala.scalajs.js.annotation.*

class A extends js.Object {
  override def hashCode(): Int = 1 // error
  override def equals(obj: Any): Boolean = false // error

  // this one works as expected (so allowed)
  override def toString(): String = "frobber"

  /* these are allowed, since they are protected in jl.Object.
   * as a result, only the overrides can be called. So the fact that they
   * do not truly override the methods in jl.Object is not observable.
   */
  override def clone(): Object = null
  override def finalize(): Unit = ()

  // other methods in jl.Object are final.
}

@js.native
@JSGlobal
object B extends js.Object {
  override def hashCode(): Int = js.native // error
  override def equals(obj: Any): Boolean = js.native // error
}

@js.native
trait C extends js.Any {
  override def hashCode(): Int = js.native // error
  override def equals(obj: Any): Boolean = js.native // error
}

trait D extends js.Any {
  override def hashCode(): Int // error
  override def equals(obj: Any): Boolean // error
}
