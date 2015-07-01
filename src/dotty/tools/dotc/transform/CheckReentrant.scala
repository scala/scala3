package dotty.tools.dotc
package transform

import core._
import Names._
import dotty.tools.dotc.transform.TreeTransforms.{AnnotationTransformer, TransformerInfo, MiniPhaseTransform, TreeTransformer}
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
import util.CtxLazy


/** The first tree transform
 *   - ensures there are companion objects for all classes except module classes
 *   - eliminates some kinds of trees: Imports, NamedArgs
 *   - stubs out native methods
 */
class CheckReentrant extends MiniPhaseTransform { thisTransformer =>
  import ast.tpd._

  override def phaseName = "checkReentrant"

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = tp

  private var shared: Set[Symbol] = Set()

  private var seen: Set[ClassSymbol] = Set()

  private var indent: Int = 0

  private val sharableAnnot = new CtxLazy(implicit ctx =>
    ctx.requiredClass("dotty.tools.sharable"))
  private val unsharedAnnot = new CtxLazy(implicit ctx =>
    ctx.requiredClass("dotty.tools.unshared"))

  def isIgnored(sym: Symbol)(implicit ctx: Context) =
    sym.hasAnnotation(sharableAnnot()) ||
    sym.hasAnnotation(unsharedAnnot())

  def scanning(sym: Symbol)(op: => Unit)(implicit ctx: Context): Unit = {
    println(i"${"  " * indent}scanning $sym")
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
              println(i"GLOBAL ${sym.showLocated}: ${sym.info} <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
              shared += sym
            } else if (!sym.is(Method) || sym.is(Accessor | ParamAccessor)) {
              scanning(sym) {
                sym.info.widenExpr.classSymbols.foreach(addVars)
              }
            }
        for (parent <- cls.classInfo.classParents)
          addVars(parent.symbol.asClass)
      }
    }
  }

  override def transformTemplate(tree: Template)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (ctx.settings.YcheckReentrant.value && tree.symbol.owner.isStaticOwner)
      addVars(tree.symbol.owner.asClass)
    tree
  }
}