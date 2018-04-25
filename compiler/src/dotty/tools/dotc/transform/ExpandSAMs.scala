package dotty.tools.dotc
package transform

import core._
import Contexts._, Symbols._, Types._, Flags._, Decorators._, StdNames._, Constants._
import SymDenotations.SymDenotation
import MegaPhase._
import SymUtils._
import ast.untpd
import ast.Trees._
import dotty.tools.dotc.reporting.diagnostic.messages.TypeMismatch
import dotty.tools.dotc.util.Positions.Position

/** Expand SAM closures that cannot be represented by the JVM as lambdas to anonymous classes.
 *  These fall into five categories
 *
 *   1. Partial function closures, we need to generate isDefinedAt and applyOrElse methods for these.
 *   2. Closures implementing non-trait classes.
 *   3. Closures implementing classes that inherit from a class other than Object
 *      (a lambda cannot not be a run-time subtype of such a class)
 *   4. Closures that implement traits which run initialization code.
 *   5. Closures that get synthesized abstract methods in the transformation pipeline. These methods can be
 *      (1) superaccessors, (2) outer references, (3) accessors for fields.
 */
class ExpandSAMs extends MiniPhase {
  override def phaseName = "expandSAMs"

  import ast.tpd._

  /** Is the SAMType `cls` also a SAM under the rules of the platform? */
  def isPlatformSam(cls: ClassSymbol)(implicit ctx: Context): Boolean =
    ctx.platform.isSam(cls)

  override def transformBlock(tree: Block)(implicit ctx: Context): Tree = tree match {
    case Block(stats @ (fn: DefDef) :: Nil, Closure(_, fnRef, tpt)) if fnRef.symbol == fn.symbol =>
      tpt.tpe match {
        case NoType => tree // it's a plain function
        case tpe @ SAMType(_) if tpe.isRef(defn.PartialFunctionClass) =>
          val tpe1 = checkRefinements(tpe, fn.pos)
          toPartialFunction(tree, tpe1)
        case tpe @ SAMType(_) if isPlatformSam(tpe.classSymbol.asClass) =>
          checkRefinements(tpe, fn.pos)
          tree
        case tpe =>
          val tpe1 = checkRefinements(tpe, fn.pos)
          val Seq(samDenot) = tpe1.abstractTermMembers.filter(!_.symbol.isSuperAccessor)
          cpy.Block(tree)(stats,
              AnonClass(tpe1 :: Nil, fn.symbol.asTerm :: Nil, samDenot.symbol.asTerm.name :: Nil))
      }
    case _ =>
      tree
  }

  private def toPartialFunction(tree: Block, tpe: Type)(implicit ctx: Context): Tree = {
    // /** An extractor for match, either contained in a block or standalone. */
    object PartialFunctionRHS {
      def unapply(tree: Tree): Option[Match] = tree match {
        case Block(Nil, expr) => unapply(expr)
        case m: Match => Some(m)
        case _ => None
      }
    }

    val closureDef(anon @ DefDef(_, _, List(List(param)), _, _)) = tree
    anon.rhs match {
      case PartialFunctionRHS(pf) =>
        val anonSym = anon.symbol

        def overrideSym(sym: Symbol) = sym.copy(
          owner = anonSym.owner,
          flags = Synthetic | Method | Final,
          info = tpe.memberInfo(sym),
          coord = tree.pos).asTerm
        val isDefinedAtFn = overrideSym(defn.PartialFunction_isDefinedAt)
        val applyOrElseFn = overrideSym(defn.PartialFunction_applyOrElse)

        def isDefinedAtRhs(paramRefss: List[List[Tree]]) = {
          val tru = Literal(Constant(true))
          def translateCase(cdef: CaseDef) =
            cpy.CaseDef(cdef)(body = tru).changeOwner(anonSym, isDefinedAtFn)
          val paramRef = paramRefss.head.head
          val defaultValue = Literal(Constant(false))
          translateMatch(pf, paramRef, pf.cases.map(translateCase), defaultValue)
        }

        def applyOrElseRhs(paramRefss: List[List[Tree]]) = {
          val List(paramRef, defaultRef) = paramRefss.head
          def translateCase(cdef: CaseDef) =
            cdef.changeOwner(anonSym, applyOrElseFn)
          val defaultValue = defaultRef.select(nme.apply).appliedTo(paramRef)
          translateMatch(pf, paramRef, pf.cases.map(translateCase), defaultValue)
        }

        def translateMatch(tree: Match, selector: Tree, cases: List[CaseDef], defaultValue: Tree) = {
          assert(tree.selector.symbol == param.symbol)
          val selectorTpe = selector.tpe.widen
          val defaultSym = ctx.newSymbol(selector.symbol.owner, nme.WILDCARD, Synthetic, selectorTpe)
          val defaultCase =
            CaseDef(
              Bind(defaultSym, Underscore(selectorTpe)),
              EmptyTree,
              defaultValue)
          val unchecked = Annotated(selector, New(ref(defn.UncheckedAnnotType)))
          cpy.Match(tree)(unchecked, cases :+ defaultCase)
            .subst(param.symbol :: Nil, selector.symbol :: Nil)
              // Needed because  a partial function can be written as:
              // param => param match { case "foo" if foo(param) => param }
              // And we need to update all references to 'param'
        }

        val isDefinedAtDef = transformFollowingDeep(DefDef(isDefinedAtFn, isDefinedAtRhs(_)))
        val applyOrElseDef = transformFollowingDeep(DefDef(applyOrElseFn, applyOrElseRhs(_)))

        val parent = defn.AbstractPartialFunctionType.appliedTo(tpe.argInfos)
        val anonCls = AnonClass(parent :: Nil, List(isDefinedAtFn, applyOrElseFn), List(nme.isDefinedAt, nme.applyOrElse))
        cpy.Block(tree)(List(isDefinedAtDef, applyOrElseDef), anonCls)

      case _ =>
        val found = tpe.baseType(defn.FunctionClass(1))
        ctx.error(TypeMismatch(found, tpe), tree.pos)
        tree
    }
  }

  private def checkRefinements(tpe: Type, pos: Position)(implicit ctx: Context): Type = tpe.dealias match {
    case RefinedType(parent, name, _) =>
      if (name.isTermName && tpe.member(name).symbol.ownersIterator.isEmpty) // if member defined in the refinement
        ctx.error("Lambda does not define " + name, pos)
      checkRefinements(parent, pos)
    case tpe =>
      tpe
  }

}
