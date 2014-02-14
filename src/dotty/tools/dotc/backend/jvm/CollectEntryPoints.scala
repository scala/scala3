/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package dotty.tools.dotc
package backend.jvm

import ast.Trees._
import core.Phases.Phase
import core.Contexts.Context
import core.Symbols.Symbol

/**
 * Collect entry points.
 * 
 * When more dotty phases are implemented, this phase can be removed
 * and its functionality moved to an earlier phase. In scalac, it's
 * part of cleanup.
 */
object CollectEntryPoints extends Phase {
  import ast.tpd._

  def name = "entryPoints"

  def run(implicit ctx: Context): Unit = {
    (new CollectTraverser).traverse(ctx.compilationUnit.tpdTree)
  }
  
  class CollectTraverser(implicit ctx: Context) extends TreeTraverser {
    def traverse(tree: Tree) = tree match {
      case td: TypeDef if td.isClassDef =>
        if (GenBCode.isJavaEntryPoint(tree.symbol)) {
          entryPoints ::= tree.symbol
        }
        // no need to traverse class members, nested classes can't hold main.

      case _ => foldOver((), tree)
    }
  }

  private var entryPoints: List[Symbol] = Nil

  def getEntryPoints(implicit ctx: Context): List[Symbol] = {
    entryPoints sortBy ("" + _.fullName) // For predictably ordered error messages.
  }

}