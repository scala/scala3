package scala.magic

import annotation.experimental

package object compiletime {

  /** Used internally under magic: A wrapper for spec strings */
  @experimental
  inline def `$spec`(inline sc: StringContext)(inline args: Any*): Unit = ()

  /** Used internally under magic: A wrapper for backquoted references to types from
   *  spec strings.
   */
  @experimental
  def `$wrappedType`[T]: Unit = ()
}
