package dotty.tools.dotc
package transform.localopt

import core.Contexts.Context
import core.Symbols._
import core.Types._
import core.Flags._
import ast.Trees._
import scala.collection.mutable
import java.util.IdentityHashMap
import core.NameKinds.LocalOptValifyName

/** Rewrite vars as vals when possible.
 *
 *  @author gan74
 */
class Valify(val simplifyPhase: Simplify) extends Optimisation {
  import ast.tpd._

  object VarDef {
    def unapply(t: ValDef)(implicit ctx: Context): Option[(Symbol, Tree)] =
      if (isVar(t.symbol)) Some(t.symbol, t.rhs)
      else None
  }

  object VarAssign {
    def unapply(t: Assign)(implicit ctx: Context): Option[(Symbol, Tree)] =
      if (t.lhs.isInstanceOf[Ident] && isVar(t.lhs.symbol)) Some(t.lhs.symbol, t.rhs)
      else None
  }


  def clear(): Unit = ()

  def visitor(implicit ctx: Context): Tree => Unit = NoVisitor


  def transformer(implicit ctx: Context): Tree => Tree = {
    case blk @ Block(stats, expr) => 

      val valified = mutable.Map[Symbol, Symbol]()

      // did we replace anything ?
      var replaced = false

      // recursively replace vars in tree
      def transform(tree: Tree) = {
        if (valified.isEmpty) tree
        else {
          // abort the valification for vars that are modified in a nested (ie: potentially conditional) subtree
          tree.foreachSubTree { 
            case VarAssign(sym, _) if valified.contains(sym) => 
              valified -= sym
            case _ => 
          }

          if (valified.isEmpty) tree
          else new TreeMap() {
              override def transform(tree: Tree)(implicit ctx: Context): Tree = {
                // abort valification in captures
                if (tree.isDef && tree.symbol.exists && tree.symbol.is(Method)) tree
                else super.transform(tree)(ctx) match {
                  case id: Ident if valified.contains(id.symbol) => 
                    replaced = true
                    ref(valified(id.symbol))
                  case t => t
                }
              }
            }.transform(tree)
        }
      }

      def valify(sym: Symbol) = {
        val newSym = valifiedSymbol(sym);
        valified += sym -> newSym
        newSym
      }

      val newStats = stats.mapConserve {
        case t @ VarDef(sym, rhs) => 
          rhs match {
            // reuse val symbol if possible
            case id: Ident if isVal(id.symbol) => 
              valified += sym -> id.symbol
              t

            case _ =>
              val newRhs = transform(rhs)
              val newSym = valify(sym) 
              Thicket(ValDef(newSym.asTerm, newRhs.changeOwnerAfter(sym, ctx.owner, simplifyPhase)), ValDef(sym.asTerm, ref(newSym)))
          }

        case t @ VarAssign(sym, rhs) =>
          rhs match {
            // reuse val symbol if possible
            case id: Ident if isVal(id.symbol) => 
              valified += sym -> id.symbol
              t

            case _ =>
              val newRhs = transform(rhs)
              val newSym = valify(sym)
              Thicket(ValDef(newSym.asTerm, newRhs.changeOwnerAfter(sym, ctx.owner, simplifyPhase)), Assign(ref(sym), ref(newSym)))
          }

        case t => 
          transform(t)
      }

      if ((newStats ne stats) || !valified.isEmpty) {
        val newExpr = transform(expr)
        if (!replaced) blk // valification didn't do anything -> return early
        else cleanUpBlock(Block(newStats, newExpr))
      } else  blk 

    case t => t
  }

  // remove all the unused declaration and assignations 
  private def cleanUpBlock(block: Block)(implicit ctx: Context): Block = {
    // valified symbols for which the var assign should not be dropped
    val dontDrop = mutable.Set[Symbol]()
    // vars defined in this block
    val defined = mutable.Set[Symbol]()

    val valified = mutable.Map[Symbol, Symbol]()

    def lookForVars(tree: Tree) = {
      def findInUse(t: Tree) =
        t.foreachSubTree({ 
          case id: Ident if valified.contains(id.symbol) => dontDrop += valified(id.symbol)
          case _ =>
        })
      
      tree match {
        case Thicket(List(valDef: ValDef, VarDef(sym, rhs))) if rhs.symbol == valDef.symbol => 
          findInUse(valDef.rhs)
          valified += sym -> valDef.symbol
          defined += sym

        case Thicket(List(valDef: ValDef, VarAssign(sym, rhs))) if rhs.symbol == valDef.symbol => 
          findInUse(valDef.rhs)
          valified += sym -> valDef.symbol

        case t => 
          findInUse(t)
      }
    }

    // var which declaration has been dropped and that should be redeclared if still in use
    val redecl = mutable.Set[Symbol]()

    def cleanup(tree: Tree) = tree match {
      case Thicket(List(valDef: ValDef, VarDef(sym, rhs))) 
        if rhs.symbol == valDef.symbol && 
           !dontDrop.contains(valDef.symbol) && 
           defined.contains(sym) => 

        redecl += sym
        valDef

      case Thicket(List(valDef: ValDef, VarAssign(sym, rhs))) 
        if rhs.symbol == valDef.symbol && 
           defined.contains(sym) && 
           !dontDrop.contains(valDef.symbol) => 

        valDef

      case Thicket(List(valDef: ValDef, VarAssign(sym, rhs))) 
        if rhs.symbol == valDef.symbol && 
           defined.contains(sym) &&
           redecl.contains(sym) =>

        redecl -= sym
        Thicket(List(valDef, ValDef(sym.asTerm, rhs)))
      
      case t => t
    }

    block.stats.foreach(lookForVars(_))
    lookForVars(block.expr)

    val newStats = block.stats.mapConserve(cleanup(_))
    val newExpr = cleanup(block.expr)
    Block(newStats, newExpr)
  }

  private def isVar(s: Symbol)(implicit ctx: Context): Boolean =  s.is(Mutable) && !s.is(Method) && !s.owner.isClass
  private def isVal(s: Symbol)(implicit ctx: Context): Boolean = !s.is(Mutable) && !s.is(Method) && !s.owner.isClass

  private def valifiedSymbol(sym: Symbol)(implicit ctx: Context): Symbol = 
    ctx.newSymbol(
      ctx.owner,       // valified symbols can have a narrower scope than the original symbol
      LocalOptValifyName.fresh(), 
      (sym.flags &~ Mutable) | Synthetic, 
      sym.info, 
      sym.privateWithin, 
      sym.coord)
}
