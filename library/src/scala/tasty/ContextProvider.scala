package scala.tasty

import scala.annotation.implicitNotFound

@implicitNotFound("Could not find implicit tasty.ContextProvider. Default ContextProvider can be imported with `import dotty.tools.dotc.tasty.ContextProvider._`")
trait ContextProvider {

  /** Provides a Contexts that is valid during the execution of `code`.
   *  DO NOT use this context of tasty trees that where generated from a different context.
   */
  def provide[T](code: Context => T): T

}
