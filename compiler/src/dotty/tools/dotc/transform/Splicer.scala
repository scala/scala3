package dotty.tools.dotc
package transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.quoted._
import dotty.tools.dotc.interpreter._

import scala.util.control.NonFatal

import java.lang.reflect.InvocationTargetException

/** Utility class to splice quoted expressions */
object Splicer {
  import tpd._

  /** Splice the Tree for a Quoted expression. `~'(xyz)` becomes `xyz`
   *  and for `~xyz` the tree of `xyz` is interpreted for which the
   *  resulting expression is returned as a `Tree`
   */
  def splice(tree: Tree)(implicit ctx: Context): Tree = tree match {
    case Quoted(quotedTree) => quotedTree
    case _ => reflectiveSplice(tree)
  }

  /** Splice the Tree for a Quoted expression which is constructed via a reflective call to the given method */
  private def reflectiveSplice(tree: Tree)(implicit ctx: Context): Tree = {
    val interpreter = new Interpreter
    val interpreted =
      try interpreter.interpretTree[scala.quoted.Expr[_]](tree)
      catch { case ex: InvocationTargetException => handleTargetException(tree, ex); None }
    interpreted.fold(tree)(PickledQuotes.quotedExprToTree)
  }

  private def handleTargetException(tree: Tree, ex: InvocationTargetException)(implicit ctx: Context): Unit = ex.getCause match {
    case ex: scala.quoted.QuoteError => ctx.error(ex.getMessage, tree.pos)
    case NonFatal(ex) =>
      val msg =
        s"""Failed to evaluate inlined quote.
           |  Caused by: ${ex.getMessage}
           |  ${ex.getStackTrace.takeWhile(_.getClassName != "sun.reflect.NativeMethodAccessorImpl").mkString("\n  ")}
         """.stripMargin
      ctx.error(msg, tree.pos)
    case _ => throw ex
  }

}
