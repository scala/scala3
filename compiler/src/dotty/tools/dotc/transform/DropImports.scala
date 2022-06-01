package dotty.tools.dotc
package transform

import ast.tpd
import core.*
import Contexts.*
import MegaPhase.MiniPhase
import Decorators.*

/** This phase finally drops all (language-) imports.
 *  Since some of the language imports change the subtyping,
 *  we cannot check the trees before erasure. Therefore, this
 *  phase should be the last phase before erasure.
 */
object DropImports:
  val name: String = "dropImports"
  val description: String = "drop all (language-) imports."

class DropImports extends MiniPhase:
  import tpd._

  override def phaseName: String = DropImports.name

  override def description: String = DropImports.description

  override def isCheckable: Boolean = false

  override def transformOther(tree: Tree)(using Context): Tree = tree match
    case tree: Import => EmptyTree
    case _ => tree
