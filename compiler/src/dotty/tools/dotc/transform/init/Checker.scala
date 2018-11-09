package dotty.tools.dotc
package transform
package init

import core._
import MegaPhase._
import Contexts.Context
import StdNames._
import Names._
import Phases._
import ast._
import Trees._
import Flags._
import SymUtils._
import Symbols._
import Denotations._
import SymDenotations._
import Types._
import Decorators._
import DenotTransformers._
import util.Positions._
import config.Printers.init.{ println => debug }
import Constants.Constant
import collection.mutable

object Checker {
  val name = "initChecker"
}

/** This transform checks initialization is safe based on data-flow analysis
 *
 *  - Cold
 *  - Warm
 *  - Hot
 *
 *  1. A _hot_ object is fully initialized.
 *  2. All fields of a _warm_ object are assigned, but the fields may refer to non-full objects.
 *  3. A _cold_ object may have unassigned fields.
 *
 *  TODO:
 *   - check default arguments of init methods
 *   - handle tailrec calls during initialization (which captures `this`)
 */
class Checker extends MiniPhase with IdentityDenotTransformer { thisPhase =>
  import tpd._

  override def phaseName: String = Checker.name

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context): Tree = {
    if (!tree.isClassDef) return tree

    val cls = tree.symbol.asClass
    if (cls.hasAnnotation(defn.UncheckedAnnot)) return tree
    checkInit(cls, tree)

    tree
  }

  def checkInit(cls: ClassSymbol, cdef: tpd.TypeDef)(implicit ctx: Context) = {
    val tmpl = cdef.rhs.asInstanceOf[tpd.Template]

    debug("*************************************")
    debug("checking " + cls.show)
    debug("*************************************")

    val analyzer = new Analyzer

    // current class env needs special setup
    val root = Heap.createRootEnv
    val setting = Setting(root, cls.pos, ctx, analyzer, inferInit = true, anchor = cls)
    val tref = cls.typeRef
    val tp = tref.appliedTo(tref.typeParams.map(_.paramInfo))
    val obj = new ObjectValue(tp = tp, open = !cls.is(Final) && !cls.isAnonymousClass, cooking = true)
      // enhancement possible to check if there are actual children
      // and whether children are possible in other modules.

    analyzer.indexOuter(cls)(setting)

    val classValue = analyzer.classValue(cdef)(setting)
    // init check
    val constr = tmpl.constr
    val values = constr.vparamss.flatten.map { param => param.tpe.widen.value }
    val poss = constr.vparamss.flatten.map(_.pos)
    val res = classValue.init(constr.symbol, values, poss, obj)(setting)

    res.effects.foreach(_.report)
  }
}
