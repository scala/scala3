package dotty.tools
package dotc
package core

import Names._
import Contexts.Context

/** Defines a common superclass of Name and Symbol and its Term/Type variants
 *  Designators are used in reference type to identity what is referred to.
 */
object Designators {

  abstract class Designator extends util.DotClass {

    type ThisName <: Name

    /** Classifiers and casts, to be overridden in implemetations */
    def isName: Boolean = false
    def isSymbol: Boolean = false

    def isTerm(implicit ctx: Context) = false
    def isType(implicit ctx: Context) = false

    def asTerm(implicit ctx: Context): TermDesignator = unsupported("asTerm")
    def asType(implicit ctx: Context): TypeDesignator = unsupported("asType")
  }

  type TermDesignator = Designator { type ThisName = TermName }
  type TypeDesignator = Designator { type ThisName = TypeName }
}