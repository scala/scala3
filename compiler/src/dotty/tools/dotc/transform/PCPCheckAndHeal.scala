package dotty.tools.dotc
package transform

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{TreeTypeMap, tpd, untpd}
import dotty.tools.dotc.core.Constants._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.quoted._
import dotty.tools.dotc.core.NameKinds._
import dotty.tools.dotc.core.StagingContext._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.tasty.TreePickler.Hole
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans._
import dotty.tools.dotc.transform.SymUtils._
import dotty.tools.dotc.transform.TreeMapWithStages._
import dotty.tools.dotc.typer.Implicits.SearchFailureType
import dotty.tools.dotc.typer.Inliner

import scala.collection.mutable
import dotty.tools.dotc.util.SourcePosition

import scala.annotation.constructorOnly

/** Checks that the Phase Consistency Principle (PCP) holds and heals types.
 *
 *  Type healing consists in transforming a phase inconsistent type `T` into a splice of `implicitly[Type[T]]`.
 */
class PCPCheckAndHeal(@constructorOnly ictx: Context) extends TreeMapWithStages(ictx) {
  import tpd._

  override def transform(tree: Tree)(implicit ctx: Context): Tree = {
    if (tree.source != ctx.source && tree.source.exists)
      transform(tree)(ctx.withSource(tree.source))
    else tree match {
      case tree: DefDef if tree.symbol.is(Inline) && level > 0 => EmptyTree
      case _ => checkLevel(super.transform(tree))
    }
  }

  /** Transform quoted trees while maintaining phase correctness */
  override protected def transformQuotation(body: Tree, quote: Tree)(implicit ctx: Context): Tree = {
    val body1 = transform(body)(quoteContext)
    super.transformQuotation(body1, quote)
  }

  /** Transform splice
   *  - If inside a quote, transform the contents of the splice.
   *  - If inside inlined code, expand the macro code.
   *  - If inside of a macro definition, check the validity of the macro.
   */
  protected def transformSplice(body: Tree, splice: Tree)(implicit ctx: Context): Tree = {
    if (level >= 1) {
      val body1 = transform(body)(spliceContext)
      splice match {
        case Apply(fun: TypeApply, _) if splice.isTerm =>
          // Type of the splice itsel must also be healed
          // internal.Quoted.expr[F[T]](... T ...)  -->  internal.Quoted.expr[F[$t]](... T ...)
          val tp = checkType(splice.sourcePos).apply(splice.tpe.widenTermRefExpr)
          cpy.Apply(splice)(cpy.TypeApply(fun)(fun.fun, tpd.TypeTree(tp) :: Nil), body1 :: Nil)
        case splice: Select => cpy.Select(splice)(body1, splice.name)
      }
    }
    else {
      assert(enclosingInlineds.isEmpty, "unexpanded macro")
      assert(ctx.owner.isInlineMethod)
      if (Splicer.canBeSpliced(body)) { // level 0 inside an inline definition
        transform(body)(spliceContext) // Just check PCP
        splice
      }
      else { // level 0 inside an inline definition
        ctx.error(
          "Malformed macro call. The contents of the splice ${...} must call a static method and arguments must be quoted or inline.",
          splice.sourcePos)
        splice
      }
    }
  }

  /** If `tree` refers to a locally defined symbol (either directly, or in a pickled type),
   *  check that its staging level matches the current level. References to types
   *  that are phase-incorrect can still be healed as follows:
   *
   *  If `T` is a reference to a type at the wrong level, try to heal it by replacing it with
   *  `${implicitly[quoted.Type[T]]}`.
   */
  protected def checkLevel(tree: Tree)(implicit ctx: Context): Tree = {
    def checkTp(tp: Type): Type = checkType(tree.sourcePos).apply(tp)
    tree match {
      case Quoted(_) | Spliced(_)  =>
        tree
      case tree: RefTree if tree.symbol.is(InlineParam) =>
        tree
      case _: This =>
        assert(checkSymLevel(tree.symbol, tree.tpe, tree.sourcePos).isEmpty)
        tree
      case _: Ident =>
        checkSymLevel(tree.symbol, tree.tpe, tree.sourcePos) match {
          case Some(tpRef) => tpRef
          case _ => tree
        }
      case _: TypeTree | _: AppliedTypeTree | _: Apply | _: TypeApply | _: UnApply | Select(_, OuterSelectName(_, _)) =>
        tree.withType(checkTp(tree.tpe))
      case _: ValOrDefDef | _: Bind =>
        tree.symbol.info = checkTp(tree.symbol.info)
        tree
      case _: Template =>
        checkTp(tree.symbol.owner.asClass.givenSelfType)
        tree
      case _ =>
        tree
    }
  }

  /** Check and heal all named types and this-types in a given type for phase consistency. */
  private def checkType(pos: SourcePosition)(implicit ctx: Context): TypeMap = new TypeMap {
    def apply(tp: Type): Type = reporting.trace(i"check type level $tp at $level") {
      tp match {
        case tp: TypeRef if tp.symbol.isSplice =>
          if (tp.isTerm)
            ctx.error(i"splice outside quotes", pos)
          tp
        case tp: NamedType =>
          checkSymLevel(tp.symbol, tp, pos) match {
            case Some(tpRef) => tpRef.tpe
            case _ =>
              if (tp.symbol.is(Param)) tp
              else mapOver(tp)
          }
        case tp: ThisType =>
          assert(checkSymLevel(tp.cls, tp, pos).isEmpty)
          mapOver(tp)
        case _ =>
          mapOver(tp)
      }
    }
  }

  /** Check reference to `sym` for phase consistency, where `tp` is the underlying type
   *  by which we refer to `sym`. If it is an inconsistent type try construct a healed type for it.
   *
   *  @return `None` if the phase is correct or cannot be healed
   *          `Some(tree)` with the `tree` of the healed type tree for `${implicitly[quoted.Type[T]]}`
   */
  private def checkSymLevel(sym: Symbol, tp: Type, pos: SourcePosition)(implicit ctx: Context): Option[Tree] = {
    /** Is a reference to a class but not `this.type` */
    def isClassRef = sym.isClass && !tp.isInstanceOf[ThisType]

    if (sym.exists && !sym.isStaticOwner && !isClassRef && !levelOK(sym))
      tryHeal(sym, tp, pos)
    else
      None
  }

  /** Does the level of `sym` match the current level?
   *  An exception is made for inline vals in macros. These are also OK if their level
   *  is one higher than the current level, because on execution such values
   *  are constant expression trees and we can pull out the constant from the tree.
   */
  private def levelOK(sym: Symbol)(implicit ctx: Context): Boolean = levelOf(sym) match {
    case Some(l) =>
      l == level ||
        level == -1 && (
          sym == defn.TastyReflection_macroContext ||
            // here we assume that Splicer.canBeSpliced was true before going to level -1,
            // this implies that all non-inline arguments are quoted and that the following two cases are checked
            // on inline parameters or type parameters.
            sym.is(Param) ||
            sym.isClass // reference to this in inline methods
          )
    case None =>
      !sym.is(Param) || levelOK(sym.owner)
  }

  /** Try to heal phase-inconsistent reference to type `T` using a local type definition.
   *  @return None      if successful
   *  @return Some(msg) if unsuccessful where `msg` is a potentially empty error message
   *                    to be added to the "inconsistent phase" message.
   */
  protected def tryHeal(sym: Symbol, tp: Type, pos: SourcePosition)(implicit ctx: Context): Option[Tree] = {
    def levelError(errMsg: String) = {
      def symStr =
        if (!tp.isInstanceOf[ThisType]) sym.show
        else if (sym.is(ModuleClass)) sym.sourceModule.show
        else i"${sym.name}.this"
      ctx.error(
        em"""access to $symStr from wrong staging level:
            | - the definition is at level ${levelOf(sym).getOrElse(0)},
            | - but the access is at level $level.$errMsg""", pos)
      None
    }
    tp match {
      case tp: TypeRef =>
        if (level == -1) {
          assert(ctx.inInlineMethod)
          None
        } else {
          val reqType = defn.QuotedTypeType.appliedTo(tp)
          val tag = ctx.typer.inferImplicitArg(reqType, pos.span)
          tag.tpe match {
            case _: TermRef =>
              Some(tag.select(tpnme.splice))
            case _: SearchFailureType =>
              levelError(i"""
                            |
                            | The access would be accepted with the right type tag, but
                            | ${ctx.typer.missingArgMsg(tag, reqType, "")}""")
            case _ =>
              levelError(i"""
                            |
                            | The access would be accepted with an implict $reqType""")
          }
        }
      case _ =>
        levelError("")
    }
  }

}
