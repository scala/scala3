package dotty.tools.dotc
package transform

import core._
import Contexts._, Symbols._, Types._, Flags._, Decorators._, StdNames._, Constants._
import SymDenotations.SymDenotation
import TreeTransforms._
import SymUtils._
import ast.untpd
import ast.Trees._
import dotty.tools.dotc.util.Positions.Position

/** Expand SAM closures that cannot be represented by the JVM as lambdas to anonymous classes.
 *  These fall into five categories
 *
 *   1. Partial function closures, we need to generate a isDefinedAt method for these.
 *   2. Closures implementing non-trait classes.
 *   3. Closures implementing classes that inherit from a class other than Object
 *      (a lambda cannot not be a run-time subtype of such a class)
 *   4. Closures that implement traits which run initialization code.
 *   5. Closures that get synthesized abstract methods in the transformation pipeline. These methods can be
 *      (1) superaccessors, (2) outer references, (3) accessors for fields.
 */
class ExpandSAMs extends MiniPhaseTransform { thisTransformer =>
  override def phaseName = "expandSAMs"

  import ast.tpd._

  /** Is the SAMType `cls` also a SAM under the rules of the platform? */
  def isPlatformSam(cls: ClassSymbol)(implicit ctx: Context): Boolean =
    ctx.platform.isSam(cls)

  override def transformBlock(tree: Block)(implicit ctx: Context, info: TransformerInfo): Tree = tree match {
    case Block(stats @ (fn: DefDef) :: Nil, Closure(_, fnRef, tpt)) if fnRef.symbol == fn.symbol =>
      tpt.tpe match {
        case NoType => tree // it's a plain function
        case tpe @ SAMType(_) if tpe.isRef(defn.PartialFunctionClass) =>
          checkRefinements(tpe, fn.pos)
          toPartialFunction(tree)
        case tpe @ SAMType(_) if isPlatformSam(tpe.classSymbol.asClass) =>
          checkRefinements(tpe, fn.pos)
          tree
        case tpe =>
          checkRefinements(tpe, fn.pos)
          val Seq(samDenot) = tpe.abstractTermMembers.filter(!_.symbol.isSuperAccessor)
          cpy.Block(tree)(stats,
              AnonClass(tpe :: Nil, fn.symbol.asTerm :: Nil, samDenot.symbol.asTerm.name :: Nil))
      }
    case _ =>
      tree
  }

  private def toPartialFunction(tree: Block)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val Block(
          (applyDef @ DefDef(nme.ANON_FUN, Nil, List(List(param)), _, _)) :: Nil,
          Closure(_, _, tpt)) = tree
    val applyRhs: Tree = applyDef.rhs
    val applyFn = applyDef.symbol.asTerm

    val MethodTpe(paramNames, paramTypes, _) = applyFn.info
    val isDefinedAtFn = applyFn.copy(
        name  = nme.isDefinedAt,
        flags = Synthetic | Method,
        info = MethodType(paramNames, paramTypes, defn.BooleanType)).asTerm
    val tru = Literal(Constant(true))
    def isDefinedAtRhs(paramRefss: List[List[Tree]]) = applyRhs match {
      case Match(selector, cases) =>
        assert(selector.symbol == param.symbol)
        val paramRef = paramRefss.head.head
        // Again, the alternative
        //     val List(List(paramRef)) = paramRefs
        // fails with a similar self instantiation error
        def translateCase(cdef: CaseDef): CaseDef =
          cpy.CaseDef(cdef)(body = tru).changeOwner(applyFn, isDefinedAtFn)
        val defaultSym = ctx.newSymbol(isDefinedAtFn, nme.WILDCARD, Synthetic, selector.tpe.widen)
        val defaultCase =
          CaseDef(
            Bind(defaultSym, Underscore(selector.tpe.widen)),
            EmptyTree,
            Literal(Constant(false)))
        val annotated = Annotated(paramRef, New(ref(defn.UncheckedAnnotType)))
        cpy.Match(applyRhs)(annotated, cases.map(translateCase) :+ defaultCase)
      case _ =>
        tru
    }
    val isDefinedAtDef = transformFollowingDeep(DefDef(isDefinedAtFn, isDefinedAtRhs(_)))
    val anonCls = AnonClass(tpt.tpe :: Nil, List(applyFn, isDefinedAtFn), List(nme.apply, nme.isDefinedAt))
    cpy.Block(tree)(List(applyDef, isDefinedAtDef), anonCls)
  }

  private def checkRefinements(tpe: Type, pos: Position)(implicit ctx: Context): Unit = tpe match {
    case RefinedType(parent, name, _) =>
      if (name.isTermName && tpe.member(name).symbol.ownersIterator.isEmpty) // if member defined in the refinement
        ctx.error("Lambda does not define " + name, pos)
      checkRefinements(parent, pos)
    case _ =>
  }

}
