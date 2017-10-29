package dotty.tools.dotc
package transform.localopt


import core.Constants.Constant
import core.Contexts.Context
import core.Decorators._
import core.Names.Name
import core.NameKinds.LocalOptValifyName
import core.Types.Type
import core.StdNames._
import core.Symbols._
import core.Flags._
import ast.Trees._
import scala.collection.mutable
import transform.SymUtils._
import Simplify._

/** Tries to rewrite ALL vars as vals.
 *
 *  - vars that become unused are dropped 
 *  - rewrites that would create unused vals are ignored to garente a fix point
 * 
 *  @author gan74
 */
class Valify(val simplifyPhase: Simplify) extends Optimisation {
  import ast.tpd._

  def clear(): Unit = {
    usedVars.clear()
    replaceDefs.clear()
    replaceAssigns.clear()
    replaceIdents.clear()
  }

  // Trees to replace
  var replaceDefs:    mutable.Map[ValDef, List[Tree]] = mutable.Map()
  var replaceAssigns: mutable.Map[Assign, List[Tree]] = mutable.Map()
  var replaceIdents:  mutable.Map[Ident, Tree]        = mutable.Map()

  // vars that are still in use after valification
  val usedVars: mutable.Map[Symbol, Int] = mutable.Map()
  //                                 ^ count the number of non replaced usage for a give symbol


  def visitor(implicit ctx: Context): Tree => Unit =
    tree => tree match {
      case Block(stats, expr) => visitBlock(stats, expr)
      case id: Ident if isVar(id.symbol) => symbolUsed(id.symbol)
      case _ => 
    }

  def transformer(implicit ctx: Context): Tree => Tree = {
    // unused valified symbols 
    lazy val unused = replaceDefs.keySet.map(_.symbol) ++
                      replaceAssigns.keySet.map(_.lhs.symbol) --
                      replaceIdents.keySet.map(_.symbol)

    // remove unused vars
    def dropUnusedVar(tree: Tree) = 
      tree match {
        case t: ValDef if isVar(t.symbol)     && usedVars.getOrElse(t.symbol,     0) == 0 => t.rhs
        case t: Assign if isVar(t.lhs.symbol) && usedVars.getOrElse(t.lhs.symbol, 0) == 0 => t.rhs
        case t => t 
      }

    replaceDefs    = replaceDefs.map    { case (k, rpl) => (k, rpl.map(dropUnusedVar(_))) }
    replaceAssigns = replaceAssigns.map { case (k, rpl) => (k, rpl.map(dropUnusedVar(_))) }
    replaceIdents  = replaceIdents.map  { case (k, rpl) => (k, dropUnusedVar(rpl)) }

    tree => tree match {
      case t: Ident => replaceIdents.getOrElse(t, t)

      case blk @ Block(stats, expr) =>
        var newStats = stats.flatMap { 
          case t: ValDef if !unused.contains(t.symbol)     => replaceDefs.getOrElse(t, List(t))
          case t: Assign if !unused.contains(t.lhs.symbol) => replaceAssigns.getOrElse(t, List(t))
          case t => List(t)
        }

        if (newStats.size == stats.size) blk
        else Block(newStats, expr)

      case t => t
    }
  }





  private def visitBlock(stats: List[Tree], expr: Tree)(implicit ctx: Context) = {
    // var -> valified symbol
    val symbolMap: mutable.Map[Symbol, Symbol] = mutable.Map()

    // remove written vars from symbolMap, tag vars than can not be replaced
    def findWritten(tree: Tree)(implicit ctx: Context) = 
      tree.foreachSubTree { 
        case Assign(lhs, _) => symbolMap -= lhs.symbol
        case _ => 
      }

    // walk the tree and fill replaceIdents
    def visitTree(tree: Tree) = {
      findWritten(tree)
      tree.filterSubTrees(t => t.isInstanceOf[Ident]).foreach(
        id => id match {
          case id: Ident if isVar(id.symbol) => 
            symbolMap.get(id.symbol) match { 
              case Some(sym) => 
                symbolReplaced(id.symbol)
                replaceIdents(id) = ref(sym)
              case _ =>
            }
          case _ =>
        })
    }
    
    // same as visitTree but does the replacing immediatly: returns a new modified tree instead
    def transform(newSym: Symbol, oldSym: Symbol, tree: Tree) = {
      findWritten(tree)
      new TreeMap() {
        override def transform(tree: Tree)(implicit ctx: Context): Tree = {
          val innerCtx = if (tree.isDef && tree.symbol.exists) ctx.withOwner(tree.symbol) else ctx
          super.transform(tree)(innerCtx) match {
            case id: Ident if isVar(id.symbol) => 
              symbolMap.get(id.symbol) match {
                case Some(sym) => 
                  symbolReplaced(id.symbol)
                  ref(sym)
                case _ => id
              }
            case t => 
              if (t.symbol.exists && t.symbol.owner == oldSym) {
                t.changeOwnerAfter(oldSym, newSym, simplifyPhase) 
              } else t
          }
        }
      }.transform(tree)
    }

    for (stat <- stats) stat match {
      case t: Assign if isVar(t.lhs.symbol) && t.rhs.isInstanceOf[Ident] && isVal(t.rhs.symbol) => // already valified 
      case t: ValDef if isVar(t.symbol)     && t.rhs.isInstanceOf[Ident] && isVal(t.rhs.symbol) => // already valified 

      case t: Assign if isVar(t.lhs.symbol) => 
        symbolReplaced(t.lhs.symbol)
        val newSym = valifiedSymbol(t.lhs.symbol)
        replaceAssigns(t) = List(ValDef(newSym.asTerm, transform(newSym, t.symbol, t.rhs)), Assign(t.lhs, ref(newSym)))
        symbolMap(t.lhs.symbol) = newSym

      case t: ValDef if isVar(t.symbol) => 
        val newSym = valifiedSymbol(t.symbol)
        replaceDefs(t) = List(ValDef(newSym.asTerm, transform(newSym, t.symbol, t.rhs)), ValDef(t.symbol.asTerm, ref(newSym)))
        symbolMap(t.symbol) = newSym

      case t: Assign => 
        visitTree(t.rhs)
        symbolMap -= t.lhs.symbol

      case t => visitTree(t)
    }
    visitTree(expr)
  }

  private def symbolUsed(sym: Symbol) = usedVars(sym) = usedVars.getOrElse(sym, 0) + 1
  private def symbolReplaced(sym: Symbol) = usedVars(sym) = usedVars.getOrElse(sym, 0) - 1

  private def valifiedSymbol(sym: Symbol)(implicit ctx: Context) = {
    val name = LocalOptValifyName.fresh()
    ctx.newSymbol(sym.owner, name, sym.flags.&~(Mutable)|Synthetic, sym.info, sym.privateWithin, sym.coord)
  }

  private def isVar(sym: Symbol)(implicit ctx: Context) = sym.is(Mutable, Lazy) && !sym.is(Method) && !sym.owner.isClass
  private def isVal(sym: Symbol)(implicit ctx: Context) = !sym.is(Mutable) && !sym.is(Method) && !sym.owner.isClass
}
