package dotty.tools.dotc
package transform.localopt

import core.Constants.Constant
import core.Contexts.Context
import core.Decorators._
import core.Names.Name
import core.NameKinds.LocalOptInlineLocalObj
import core.Types.Type
import core.StdNames._
import core.Symbols._
import core.Flags._
import ast.Trees._
import scala.collection.mutable
import transform.SymUtils._
import config.Printers.simplify
import Simplify._

/** Rewrite fields of local instances as vals.
 *
 *  If a local instance does not escape the local scope, it will be removed
 *  later by DropNoEffects, thus implementing the equivalent of (local) multi
 *  parameter value classes. The main motivation for this transformation is to
 *  get ride of the intermediate tuples object somes created when pattern
 *  matching on Scala2 case classes.
 */
class InlineLocalObjects(val simplifyPhase: Simplify) extends Optimisation {
  import ast.tpd._

  // ValDefs whose rhs is a case class instantiation: potential candidates.
  val candidates = mutable.HashSet[Symbol]()

  // ValDefs whose lhs is used with `._1` (or any getter call).
  val gettersCalled = mutable.HashSet[Symbol]()

  // Map from class to new fields, initialised between visitor and transformer.
  var newFieldsMapping: Map[Symbol, Map[Symbol, Symbol]] = null
  //                   |           |       |
  //                   |           |       New fields, replacements these getters
  //                   |           Usages of getters of these classes
  //                   ValDefs of the classes that are being torn apart; = candidates.intersect(gettersCalled)

  def clear(): Unit = {
    candidates.clear()
    gettersCalled.clear()
    newFieldsMapping = null
  }

  def initNewFieldsMapping()(implicit ctx: Context): Unit =
    if (newFieldsMapping == null) {
      newFieldsMapping = candidates.intersect(gettersCalled).map { refVal =>
        val accessors = refVal.info.classSymbol.caseAccessors.filter(_.isGetter)
        val newLocals = accessors.map { x =>
          val owner: Symbol  = refVal.owner
          val name:  Name    = LocalOptInlineLocalObj.fresh()
          val flags: FlagSet = Synthetic
          val info:  Type    = x.asSeenFrom(refVal.info).info.finalResultType.widenDealias
          ctx.newSymbol(owner, name, flags, info)
        }
        (refVal, accessors.zip(newLocals).toMap)
      }.toMap
    }

  // Pattern for candidates to this optimisation: ValDefs where the rhs is an
  // immutable case class instantiation.
  object NewCaseClassValDef {
    def unapply(t: ValDef)(implicit ctx: Context): Option[(Tree, List[Tree])] =
      t.rhs match {
        case Apply(fun, args)
          if t.symbol.info.classSymbol.is(CaseClass)                          && // is rhs a case class?
             !t.symbol.is(Lazy | Mutable)                                     && // is lhs a val?
             !t.symbol.info.classSymbol.caseAccessors.exists(_.is(Mutable))   && // is the case class immutable?
             fun.symbol.isConstructor                                         && // is rhs a new?
             t.tpe.widenDealias == t.symbol.info.finalResultType.widenDealias => // no case class inheritance or enums
          Some((fun, args))
        case _ => None
      }
  }

  def visitor(implicit ctx: Context): Tree => Unit = {
    case t @ NewCaseClassValDef(fun, args) =>
      candidates += t.symbol
    case t @ Select(qual, _) if isImmutableAccessor(t) =>
      gettersCalled += qual.symbol
    case _ =>
  }

  def transformer(implicit ctx: Context): Tree => Tree = {
    initNewFieldsMapping();
    {
      case t @ NewCaseClassValDef(fun, args) if newFieldsMapping.contains(t.symbol) =>
        val newFields     = newFieldsMapping(t.symbol).values.toList
        val newFieldsDefs = newFields.zip(args).map { case (nf, arg) =>
          val rhs = arg.changeOwnerAfter(t.symbol, nf.symbol, simplifyPhase)
          ValDef(nf.asTerm, rhs)
        }
        val recreate      = cpy.ValDef(t)(rhs = fun.appliedToArgs(newFields.map(x => ref(x))))
        simplify.println(s"Replacing ${t.symbol.fullName} with stack-allocated fields ($newFields)")
        Thicket(newFieldsDefs :+ recreate)

      case t @ Select(rec, _) if isImmutableAccessor(t) =>
        newFieldsMapping.getOrElse(rec.symbol, Map.empty).get(t.symbol) match {
          case None         => t
          case Some(newSym) => ref(newSym)
        }

      case t => t
    }
  }
}
