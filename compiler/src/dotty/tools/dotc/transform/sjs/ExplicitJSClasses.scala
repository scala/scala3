package dotty.tools
package dotc
package transform
package sjs

import MegaPhase._
import core.DenotTransformers._
import core.Symbols._
import core.Contexts._
import core.Phases._
import core.Types._
import core.Flags._
import core.Decorators._
import core.StdNames.nme
import core.Names._
import core.NameOps._
import ast.Trees._
import SymUtils._
import dotty.tools.dotc.ast.tpd

import dotty.tools.backend.sjs.JSDefinitions.jsdefn

/** This phase makes all JS classes explicit (their definitions and references to them).
 *
 *  Ultimately, this will be the equivalent of the two phases `ExplicitInnerJS`
 *  and `ExplicitLocalJS` from Scala 2. Currently, this phase only performs the
 *  following transformations:
 *
 *  - Rewrite `js.constructorOf[T]` into `scala.scalajs.runtime.constructorOf(classOf[T])`,
 *    where the `classOf[T]` is represented as a `Literal`.
 */
class ExplicitJSClasses extends MiniPhase with InfoTransformer { thisPhase =>
  import ExplicitJSClasses._
  import ast.tpd._

  override def phaseName: String = ExplicitJSClasses.name

  override def isEnabled(using Context): Boolean =
    ctx.settings.scalajs.value

  override def runsAfter: Set[String] = Set(PatternMatcher.name, HoistSuperArgs.name)

  override def changesMembers: Boolean = true // the phase adds fields for inner JS classes

  override def transformInfo(tp: Type, sym: Symbol)(using Context): Type = {
    // Currently we don't do anything here. Eventually we'll add fields for inner JS classes.
    tp
  }

  override def infoMayChange(sym: Symbol)(using Context): Boolean =
    sym.isClass && !sym.is(JavaDefined)

  override def transformTypeApply(tree: TypeApply)(using Context): tpd.Tree = {
    tree match {
      case TypeApply(fun, tpt :: Nil) if fun.symbol == jsdefn.JSPackage_constructorOf =>
        ref(jsdefn.Runtime_constructorOf).appliedTo(clsOf(tpt.tpe))
      case _ =>
        tree
    }
  }
}

object ExplicitJSClasses {
  val name: String = "explicitJSClasses"
}
