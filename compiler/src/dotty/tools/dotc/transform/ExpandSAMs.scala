package dotty.tools
package dotc
package transform

import core.*
import core.TypeErasure.erasure
import Scopes.newScope
import Contexts.*, Symbols.*, Types.*, Flags.*, Decorators.*, StdNames.*, Constants.*
import MegaPhase.*
import Names.TypeName

import NullOpsDecorator.*
import ast.untpd
import scala.collection.mutable.ListBuffer

/** Expand SAM closures that cannot be represented by the JVM as lambdas to anonymous classes.
 *  These fall into five categories
 *
 *   1. Partial function closures, we need to generate isDefinedAt and applyOrElse methods for these.
 *   2. Closures implementing non-trait classes
 *   3. Closures implementing classes that inherit from a class other than Object
 *      (a lambda cannot be a run-time subtype of such a class)
 *   4. Closures that implement traits which run initialization code.
 *   5. Closures that get synthesized abstract methods in the transformation pipeline. These methods can be
 *      (1) superaccessors, (2) outer references, (3) accessors for fields.
 *
 *  However, implicit function types do not count as SAM types.
 */
object ExpandSAMs:
  val name: String = "expandSAMs"
  val description: String = "expand SAM closures to anonymous classes"

  /** Is the SAMType `cls` also a SAM under the rules of the platform? */
  def isPlatformSam(cls: ClassSymbol)(using Context): Boolean =
    ctx.platform.isSam(cls)

  def needsWrapperClass(tpe: Type)(using Context): Boolean =
    tpe.classSymbol match
      case cls: ClassSymbol => !isPlatformSam(cls) || cls == defn.PartialFunctionClass
      case _ => false

class ExpandSAMs extends MiniPhase:
  import ast.tpd.*

  override def phaseName: String = ExpandSAMs.name

  override def description: String = ExpandSAMs.description

  override def transformBlock(tree: Block)(using Context): Tree = tree match {
    case Block(stats @ (fn: DefDef) :: Nil, Closure(_, fnRef, tpt)) if fnRef.symbol == fn.symbol =>
      tpt.tpe match {
        case NoType =>
          tree // it's a plain function
        case tpe if defn.isContextFunctionType(tpe) =>
          tree
        case SAMType(_, tpe) if tpe.isRef(defn.PartialFunctionClass) =>
          toPartialFunction(tree, tpe)
        case SAMType(_, tpe) if ExpandSAMs.isPlatformSam(tpe.classSymbol.asClass) =>
          tree
        case tpe =>
          // A SAM type is allowed to have type aliases refinements (see
          // SAMType#samParent) which must be converted into type members if
          // the closure is desugared into a class.
          val refinements = ListBuffer.empty[(TypeName, TypeAlias)]
          def collectAndStripRefinements(tp: Type): Type = tp match
            case RefinedType(parent, name, info: TypeAlias) =>
              val res = collectAndStripRefinements(parent)
              refinements += ((name.asTypeName, info))
              res
            case _ => tp
          val tpe1 = collectAndStripRefinements(tpe)
          val Seq(samDenot) = tpe1.possibleSamMethods
          val implSym = fn.symbol.asTerm

          // Check if SAM param erases to array but impl param doesn't (see #23179).
          // E.g. SAM expects Array[AnyRef] (erases to Array[Object]) but impl has
          // Array[? >: AnyRef] (erases to Object). In this case we need to create
          // a wrapper with the SAM signature that casts arguments before forwarding.
          val samInfo = samDenot.info
          val implInfo = implSym.info
          val needsArrayAdaptation = (samInfo, implInfo) match
            case (samMt: MethodType, implMt: MethodType) =>
              samMt.paramInfos.lazyZip(implMt.paramInfos).exists { (samPt, implPt) =>
                erasure(samPt).isInstanceOf[JavaArrayType] && !erasure(implPt).isInstanceOf[JavaArrayType]
              }
            case _ => false

          if needsArrayAdaptation then
            // Create a wrapper method with SAM's signature that casts args to impl's types
            val wrapperSym = newSymbol(
              implSym.owner, implSym.name.asTermName, Synthetic | Method,
              samInfo, coord = implSym.span).entered.asTerm
            val wrapperDef = DefDef(wrapperSym, paramss => {
              val implMt = implInfo.stripPoly.asInstanceOf[MethodType]
              val adaptedArgs = paramss.head.lazyZip(implMt.paramInfos).map { (arg, implPt) =>
                if arg.tpe =:= implPt then arg else arg.cast(implPt)
              }
              ref(implSym).appliedToTermArgs(adaptedArgs)
            })
            cpy.Block(tree)(fn :: wrapperDef :: Nil,
              transformFollowingDeep:
                AnonClass(List(tpe1),
                  List(samDenot.symbol.asTerm.name -> wrapperSym),
                  refinements.toList,
                  adaptVarargs = true
                )
            )
          else
            cpy.Block(tree)(stats,
              transformFollowingDeep:
                AnonClass(List(tpe1),
                  List(samDenot.symbol.asTerm.name -> implSym),
                  refinements.toList,
                  adaptVarargs = true
                )
            )
      }
    case _ =>
      tree
  }

  /** A pattern-matching anonymous function:
   *
   *  ```
   *  val x: PartialFunction[A, B] = { case C1 => E1; ...; case Cn => En }
   *  ```
   *  or
   *  ```
   *  x => e(x) { case C1 => E1; ...; case Cn => En }
   *  ```
   *  where the expression `e(x)` may be trivially `x`
   *
   *  which desugars to:
   *
   *  ```
   *  val x: PartialFunction[A, B] = {
   *    def $anonfun(x: A): B = e(x) match { case C1 => E1; ...; case Cn => En }
   *    closure($anonfun: PartialFunction[A, B])
   *  }
   *  ```
   *  where the expression `e(x)` defaults to `x` for a simple block of cases
   *
   *  is expanded to an anonymous class:
   *
   *  ```
   *  val x: PartialFunction[A, B] = {
   *    class $anon extends AbstractPartialFunction[A, B] {
   *      final def isDefinedAt(x: A): Boolean = e(x) match {
   *        case C1 => true
   *        ...
   *        case Cn => true
   *        case _  => false
   *      }
   *
   *      final def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1 = e(x) match {
   *        case C1 => E1
   *        ...
   *        case Cn => En
   *        case _  => default(x)
   *      }
   *    }
   *
   *    new $anon
   *  }
   *  ```
   */
  private def toPartialFunction(tree: Block, tpe: Type)(using Context): Tree =
    val closureDef(anon @ DefDef(_, List(List(param)), _, _)) = tree: @unchecked

    // The right hand side from which to construct the partial function. This is always a Match.
    // If the original rhs is already a Match (possibly in braces), return that.
    // Otherwise construct a match `x match case _ => rhs` where `x` is the parameter of the closure.
    def partialFunRHS(tree: Tree): Match =
      inline def checkMatch(): Unit =
        tree match
        case Block(_, m: Match) => report.warning(reporting.MatchIsNotPartialFunction(), m.srcPos)
        case _ =>
      tree match
      case m: Match => m
      case Block(Nil, expr) => partialFunRHS(expr)
      case _ =>
        checkMatch()
        Match(ref(param.symbol),
          CaseDef(untpd.Ident(nme.WILDCARD).withType(param.symbol.info), EmptyTree, tree) :: Nil)

    val pfRHS = partialFunRHS(anon.rhs)
    val anonSym = anon.symbol
    val anonTpe = anon.tpe.widen
    val parents = List(
      defn.AbstractPartialFunctionClass.typeRef.appliedTo(anonTpe.firstParamTypes.head, anonTpe.resultType),
      defn.SerializableType)

    AnonClass(anonSym.owner, parents, tree.span): pfSym =>
      def overrideSym(sym: Symbol) = sym.copy(
        owner = pfSym,
        flags = Synthetic | Method | Final | Override,
        info = tpe.memberInfo(sym),
        coord = tree.span).asTerm.entered
      val isDefinedAtFn = overrideSym(defn.PartialFunction_isDefinedAt)
      val applyOrElseFn = overrideSym(defn.PartialFunction_applyOrElse)

      def translateMatch(owner: Symbol)(pfParam: Symbol, cases: List[CaseDef], defaultValue: Tree)(using Context) =
        val tree: Match = pfRHS
        val selector = tree.selector
        val cases1 = if cases.exists(isDefaultCase) then cases
        else
          val selectorTpe = selector.tpe.widen
          val defaultSym = newSymbol(pfParam.owner, nme.WILDCARD, SyntheticCase, selectorTpe)
          val defaultCase = CaseDef(Bind(defaultSym, Underscore(selectorTpe)), EmptyTree, defaultValue)
          cases :+ defaultCase
        cpy.Match(tree)(selector, cases1)
          .subst(param.symbol :: Nil, pfParam :: Nil)
            // Needed because a partial function can be written as:
            // param => param match { case "foo" if foo(param) => param }
            // And we need to update all references to 'param'
          .changeOwner(anonSym, owner)

      def isDefinedAtRhs(paramRefss: List[List[Tree]])(using Context) =
        val tru = Literal(Constant(true))
        def translateCase(cdef: CaseDef): CaseDef =
          val body1 = cdef.body match
            case b: SubMatch => cpy.Match(b)(b.selector, b.cases.map(translateCase))
            case _ => tru
          cpy.CaseDef(cdef)(body = body1)
        val paramRef = paramRefss.head.head
        val defaultValue = Literal(Constant(false))
        translateMatch(isDefinedAtFn)(paramRef.symbol, pfRHS.cases.map(translateCase), defaultValue)

      def applyOrElseRhs(paramRefss: List[List[Tree]])(using Context) =
        val List(paramRef, defaultRef) = paramRefss(1)
        val defaultValue = defaultRef.select(nme.apply).appliedTo(paramRef)
        translateMatch(applyOrElseFn)(paramRef.symbol, pfRHS.cases, defaultValue)

      val isDefinedAtDef = transformFollowingDeep:
        DefDef(isDefinedAtFn, isDefinedAtRhs(_)(using ctx.withOwner(isDefinedAtFn)))
      val applyOrElseDef = transformFollowingDeep:
        DefDef(applyOrElseFn, applyOrElseRhs(_)(using ctx.withOwner(applyOrElseFn)))
      List(isDefinedAtDef, applyOrElseDef)
  end toPartialFunction
end ExpandSAMs
