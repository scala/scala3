package dotty.annotation

import scala.annotation.Annotation

/** Unlike scala.inline, this one forces inlining in the Typer
 *  Should be replaced by keyword when we switch over completely to dotty
 */
class inline extends Annotation