package scala.magic

import annotation.experimental

@experimental
object compiletime {

  /** Used internally under magic: A wrapper for spec strings */
  inline def `$spec`(inline sc: StringContext)(inline args: Any*): Unit = ()

  /** Used internally under magic: A wrapper for backquoted references to types from
   *  spec strings.
   */
  def `$wrappedType`[T]: Unit = ()
}
