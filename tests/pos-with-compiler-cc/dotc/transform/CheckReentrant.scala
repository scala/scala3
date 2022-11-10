package dotty.tools.dotc
package transform

import core._
import dotty.tools.dotc.transform.MegaPhase._
import Flags._
import Contexts._
import Symbols._
import Decorators._

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
class CheckReentrant extends MiniPhase {
  import ast.tpd._

  override def phaseName: String = CheckReentrant.name

  override def description: String = CheckReentrant.description

  private var shared: Set[Symbol] = Set()
  private var seen: Set[ClassSymbol] = Set()
  private var indent: Int = 0

  private val sharableAnnot = new CtxLazy(
    requiredClass("scala.annotation.internal.sharable"))
  private val unsharedAnnot = new CtxLazy(
    requiredClass("scala.annotation.internal.unshared"))

  private val scalaJSIRPackageClass = new CtxLazy(
    getPackageClassIfDefined("org.scalajs.ir"))

  def isIgnored(sym: Symbol)(using Context): Boolean =
    sym.hasAnnotation(sharableAnnot()) ||
    sym.hasAnnotation(unsharedAnnot()) ||
    sym.topLevelClass.owner == scalaJSIRPackageClass()
      // We would add @sharable annotations on ScalaJSVersions and
      // VersionChecks but we do not have control over that code

  def scanning(sym: Symbol)(op: => Unit)(using Context): Unit = {
    report.log(i"${"  " * indent}scanning $sym")
    indent += 1
    try op
    finally indent -= 1
  }

  def addVars(cls: ClassSymbol)(using Context): Unit =
    if (!seen.contains(cls) && !isIgnored(cls)) {
      seen += cls
      scanning(cls) {
        for (sym <- cls.classInfo.decls)
          if (sym.isTerm && !sym.isSetter && !isIgnored(sym))
            if (sym.is(Mutable)) {
              report.error(
                i"""possible data race involving globally reachable ${sym.showLocated}: ${sym.info}
                   |  use -Ylog:checkReentrant+ to find out more about why the variable is reachable.""")
              shared += sym
            }
            else if (!sym.is(Method) || sym.isOneOf(Accessor | ParamAccessor))
              scanning(sym) {
                sym.info.widenExpr.classSymbols.foreach(addVars)
              }
        for (parent <- cls.parentSyms)
          addVars(parent.asClass)
      }
    }

  override def transformTemplate(tree: Template)(using Context): Tree = {
    if (ctx.settings.YcheckReentrant.value && tree.symbol.owner.isStaticOwner)
      addVars(tree.symbol.owner.asClass)
    tree
  }
}

object CheckReentrant:
  val name: String = "checkReentrant"
  val description: String = "check no data races involving global vars"
