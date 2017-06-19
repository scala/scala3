package dotty.tools.dotc
package transform.localopt

import core.Constants.Constant
import core.Contexts.Context
import core.Decorators._
import core.Names.Name
import core.Types.Type
import core.NameOps._
import core.StdNames._
import core.Symbols._
import core.Flags._
import ast.Trees._
import scala.collection.mutable
import transform.SymUtils._
import config.Printers.simplify

/** Inline case classes as vals.
 *
 *  In other words, implements (local) multi parameter value classes. The main
 *  motivation is to get ride of all the intermediate tuples created by the
 *  pattern matcher.
 */
class InlineLocalObjects extends Optimisation {
  import ast.tpd._

  // In the end only calls constructor. Reason for unconditional inlining
  val hasPerfectRHS = mutable.HashMap[Symbol, Boolean]()

  // If all values have perfect RHS than key has perfect RHS
  val checkGood = mutable.HashMap[Symbol, Set[Symbol]]()

  val forwarderWritesTo = mutable.HashMap[Symbol, Symbol]()

  val gettersCalled = mutable.HashSet[Symbol]()

  def symbolAccessors(s: Symbol)(implicit ctx: Context): List[Symbol] = {
    val accessors = s.info.classSymbol.caseAccessors.filter(_.isGetter)
    if (accessors.isEmpty)
      s.info.classSymbol.caseAccessors
    else accessors
  }

  def followTailPerfect(t: Tree, symbol: Symbol)(implicit ctx: Context): Unit = {
    t match {
      case Block(_, expr) =>
        followTailPerfect(expr, symbol)

      case If(_, thenp, elsep) =>
        followTailPerfect(thenp, symbol)
        followTailPerfect(elsep, symbol)

      case Apply(fun, _) if fun.symbol.isConstructor && t.tpe.widenDealias == symbol.info.widenDealias.finalResultType.widenDealias =>
        hasPerfectRHS(symbol) = true

      case Apply(fun, _) if fun.symbol.is(Label) && (fun.symbol ne symbol) =>
        checkGood.put(symbol, checkGood.getOrElse(symbol, Set.empty) + fun.symbol)
        // assert(forwarderWritesTo.getOrElse(t.symbol, symbol) == symbol)
        forwarderWritesTo(t.symbol) = symbol

      case t: Ident if !t.symbol.owner.isClass && (t.symbol ne symbol) =>
        checkGood.put(symbol, checkGood.getOrElse(symbol, Set.empty) + t.symbol)

      case _ =>
    }
  }

  def visitor(implicit ctx: Context): Tree => Unit = {
    case t: ValDef if (t.symbol.info.classSymbol is CaseClass) &&
                      !t.symbol.is(Lazy)                       &&
                      !t.symbol.info.classSymbol.caseAccessors.exists(_.is(Mutable)) =>
      followTailPerfect(t.rhs, t.symbol)

    case Assign(lhs, rhs) if !lhs.symbol.owner.isClass =>
      checkGood.put(lhs.symbol, checkGood.getOrElse(lhs.symbol, Set.empty) + rhs.symbol)

    case t @ Select(qual, _) if
      (t.symbol.isGetter && !t.symbol.is(Mutable | Lazy)) ||
      (t.symbol.maybeOwner.derivesFrom(defn.ProductClass) && t.symbol.maybeOwner.is(CaseClass) && t.symbol.name.isSelectorName) ||
      (t.symbol.is(CaseAccessor) && !t.symbol.is(Mutable)) =>
      gettersCalled(qual.symbol) = true

    case t: DefDef if t.symbol.is(Label) =>
      followTailPerfect(t.rhs, t.symbol)

    case _ =>
  }

  def transformer(implicit ctx: Context): Tree => Tree = {
    var hasChanged = true
    while (hasChanged) {
      hasChanged = false
      checkGood.foreach { case (key, values) =>
        values.foreach { value =>
          if (hasPerfectRHS.getOrElse(key, false)) {
            hasChanged = !hasPerfectRHS.put(value, true).getOrElse(false)
          }
        }
      }
    }

    val newMappings: Map[Symbol, Map[Symbol, Symbol]] =
      hasPerfectRHS.iterator
          .map(_._1)
          .filter(x => !x.is(Method | Label) && gettersCalled.contains(x.symbol) && x.symbol.info.classSymbol.is(CaseClass))
          .map { refVal =>
            simplify.println(s"replacing ${refVal.symbol.fullName} with stack-allocated fields")
            var accessors = refVal.info.classSymbol.caseAccessors.filter(_.isGetter) // TODO: drop mutable ones
            if (accessors.isEmpty) accessors = refVal.info.classSymbol.caseAccessors

            val productAccessors = (1 to accessors.length).map { i =>
              refVal.info.member(nme.productAccessorName(i)).symbol
            } // TODO: disambiguate

            val newLocals = accessors.map { x =>
              // TODO: it would be nice to have an additional optimisation that
              // TODO: is capable of turning those mutable ones into immutable in common cases
              val owner: Symbol  = ctx.owner.enclosingMethod
              val name:  Name    = (refVal.name + "$" + x.name).toTermName
              val flags: FlagSet = Synthetic | Mutable
              val info:  Type    = x.asSeenFrom(refVal.info).info.finalResultType.widenDealias
              ctx.newSymbol(owner, name, flags, info)
            }
            val fieldMapping = accessors zip newLocals
            val productMappings = productAccessors zip newLocals
            (refVal, (fieldMapping ++ productMappings).toMap)
          }.toMap

    val toSplit: mutable.Set[Symbol] = mutable.Set.empty ++ newMappings.keySet

    def splitWrites(t: Tree, target: Symbol): Tree = {
      t match {
        case tree @ Block(stats, expr) =>
          cpy.Block(tree)(stats, splitWrites(expr, target))

        case tree @ If(_, thenp, elsep) =>
          cpy.If(tree)(thenp = splitWrites(thenp, target), elsep =  splitWrites(elsep, target))

        case Apply(sel, args) if sel.symbol.isConstructor && t.tpe.widenDealias == target.info.widenDealias.finalResultType.widenDealias =>
          val fieldsByAccessors = newMappings(target)
          var accessors = symbolAccessors(target)
          val assigns = (accessors zip args).map(x => ref(fieldsByAccessors(x._1)).becomes(x._2))
          val recreate = sel.appliedToArgs(accessors.map(x => ref(fieldsByAccessors(x))))
          Block(assigns, recreate)

        case Apply(fun, _) if fun.symbol.is(Label) =>
          t // Do nothing. It will do on its own.

        case t: Ident if !t.symbol.owner.isClass && newMappings.contains(t.symbol) && t.symbol.info.classSymbol == target.info.classSymbol =>
          val fieldsByAccessorslhs = newMappings(target)
          val fieldsByAccessorsrhs = newMappings(t.symbol)
          val accessors = symbolAccessors(target)
          val assigns = accessors.map(x => ref(fieldsByAccessorslhs(x)).becomes(ref(fieldsByAccessorsrhs(x))))
          Block(assigns, t)
          // If `t` is itself split, push writes.

        case _ =>
          evalOnce(t){ev =>
            if (ev.tpe.derivesFrom(defn.NothingClass)) ev
            else {
              val fieldsByAccessors = newMappings(target)
              val accessors = symbolAccessors(target)
              val assigns = accessors.map(x => ref(fieldsByAccessors(x)).becomes(ev.select(x)))
              Block(assigns, ev)
            }
          } // Need to eval-once and update fields.
      }
    }

    def followCases(t: Symbol, limit: Int = 0): Symbol = if (t.symbol.is(Label)) {
      // TODO: this can create cycles, see ./tests/pos/rbtree.scala
      if (limit > 100 && limit > forwarderWritesTo.size + 1) NoSymbol
        // There may be cycles in labels, that never in the end write to a valdef(the value is always on stack)
        // there's not much we can do here, except finding such cases and bailing out
        // there may not be a cycle bigger that hashmapSize > 1
      else followCases(forwarderWritesTo.getOrElse(t.symbol, NoSymbol), limit + 1)
    } else t

    hasPerfectRHS.clear()
    // checkGood.clear()
    gettersCalled.clear()

    val res: Tree => Tree = {
      case t: DefDef if t.symbol.is(Label) =>
        newMappings.get(followCases(t.symbol)) match {
          case Some(mappings) =>
            cpy.DefDef(t)(rhs = splitWrites(t.rhs, followCases(t.symbol)))
          case _ => t
        }

      case t: ValDef if toSplit.contains(t.symbol) =>
        toSplit -= t.symbol
        // Break ValDef apart into fields + boxed value
        val newFields = newMappings(t.symbol).values.toSet
        Thicket(
          newFields.map(x => ValDef(x.asTerm, defaultValue(x.symbol.info.widenDealias))).toList :::
            List(cpy.ValDef(t)(rhs = splitWrites(t.rhs, t.symbol))))

      case t: Assign =>
        newMappings.get(t.lhs.symbol) match {
          case None =>   t
          case Some(mapping) =>
            val updates = mapping.filter(x => x._1.is(CaseAccessor)).map(x => ref(x._2).becomes(ref(t.lhs.symbol).select(x._1))).toList
            Thicket(t :: updates)
        }

      case t @ Select(rec, _) if (t.symbol.isGetter && !t.symbol.is(Mutable | Lazy)) ||
        (t.symbol.maybeOwner.derivesFrom(defn.ProductClass) && t.symbol.owner.is(CaseClass) && t.symbol.name.isSelectorName) ||
        (t.symbol.is(CaseAccessor) && !t.symbol.is(Mutable)) =>
        newMappings.getOrElse(rec.symbol, Map.empty).get(t.symbol) match {
          case None => t
          case Some(newSym) => ref(newSym)
        }

      case t => t
    }
    res
  }
}
