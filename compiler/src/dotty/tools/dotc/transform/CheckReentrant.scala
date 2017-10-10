package dotty.tools.dotc
package transform

import core._
import Names._
import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, MiniPhaseTransform, TreeTransformer}
import ast.Trees._
import Flags._
import Types._
import Constants.Constant
import Contexts.Context
import Symbols._
import SymDenotations._
import Decorators._
import dotty.tools.dotc.core.Annotations.ConcreteAnnotation
import dotty.tools.dotc.core.Denotations.SingleDenotation
import scala.collection.mutable
import DenotTransformers._
import typer.Checking
import Names.Name
import NameOps._
import StdNames._


/** A no-op transform that checks whether the compiled sources are re-entrant.
 *  If -Ycheck:reentrant is set, the phase makes sure that there are no variables
 *  that are accessible from a global object. It excludes from checking paths that
 *  are labeled with one of the annotations
 *
 *      @sharable   Indicating a class or val can be safely shared
 *      @unshared   Indicating an object will not be accessed from multiple threads
 *
 *  Currently the analysis is only intended to check the dotty compiler itself. To make
 *  it generally useful we'd need to add at least the following:
 *
 *   - Handle polymorphic instantiation: We might instantiate a generic class
 *     with a type that contains vars. If the class contains fields of the generic
 *     type, this may constitute a path to a shared var, which currently goes undetected.
 *   - Handle arrays: Array elements are currently ignored because they are often used
 *     in an immutable way anyway. To do better, it would be helpful to have a type
 *     for immutable array.
 */
class CheckReentrant extends MiniPhaseTransform { thisTransformer =>
  import ast.tpd._

  override def phaseName = "checkReentrant"

  private[this] var shared: Set[Symbol] = Set()
  private[this] var seen: Set[ClassSymbol] = Set()
  private[this] var indent: Int = 0

  private val sharableAnnot = new CtxLazy(implicit ctx =>
    ctx.requiredClass("dotty.tools.sharable"))
  private val unsharedAnnot = new CtxLazy(implicit ctx =>
    ctx.requiredClass("dotty.tools.unshared"))

  def isIgnored(sym: Symbol)(implicit ctx: Context) =
    sym.hasAnnotation(sharableAnnot()) ||
    sym.hasAnnotation(unsharedAnnot())

  def scanning(sym: Symbol)(op: => Unit)(implicit ctx: Context): Unit = {
    ctx.log(i"${"  " * indent}scanning $sym")
    indent += 1
    try op
    finally indent -= 1
  }

  def addVars(cls: ClassSymbol)(implicit ctx: Context): Unit = {
    if (!seen.contains(cls) && !isIgnored(cls)) {
      seen += cls
      scanning(cls) {
        for (sym <- cls.classInfo.decls)
          if (sym.isTerm && !sym.isSetter && !isIgnored(sym))
            if (sym.is(Mutable)) {
              ctx.error(
                i"""possible data race involving globally reachable ${sym.showLocated}: ${sym.info}
                   |  use -Ylog:checkReentrant+ to find out more about why the variable is reachable.""")
              shared += sym
            } else if (!sym.is(Method) || sym.is(Accessor | ParamAccessor)) {
              scanning(sym) {
                sym.info.widenExpr.classSymbols.foreach(addVars)
              }
            }
        for (parent <- cls.classInfo.classParents)
          addVars(parent.typeSymbol.asClass)
      }
    }
  }

  override def transformTemplate(tree: Template)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (ctx.settings.YcheckReentrant.value && tree.symbol.owner.isStaticOwner)
      addVars(tree.symbol.owner.asClass)
    tree
  }
}