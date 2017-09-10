package dotty.tools
package dotc
package core

import Names._

/** Defines a common superclass of Name and Symbol and its Term/Type variants
 *  Designators are used in reference type to identity what is referred to.
 */
object Designators {

  abstract class Designator extends util.DotClass {

    type ThisName <: Name

    /** Classifiers, to be overridden in implemetations */
    def isName: Boolean = false
    def isSymbol: Boolean = false

    def isTermName: Boolean = false
    def isTypeName: Boolean = false
  }

  type TermDesignator = Designator { type ThisName = TermName }
  type TypeDesignator = Designator { type ThisName = TypeName }
}