package scala.internal.tasty

import scala.quoted.QuoteContext
import scala.tasty.reflect._
import scala.internal.quoted.Unpickler

/** Part of the reflection interface that needs to be implemented by the compiler */
trait CompilerInterface { self: scala.tasty.Reflection =>

  //////////////////////
  // QUOTE UNPICKLING //
  //////////////////////

  /** Unpickle `repr` which represents a pickled `Expr` tree,
   *  replacing splice nodes with `args`
   */
  def unpickleExpr(repr: Unpickler.PickledQuote, args: Unpickler.PickledArgs): Term

  /** Unpickle `repr` which represents a pickled `Type` tree,
   *  replacing splice nodes with `args`
   */
  def unpickleType(repr: Unpickler.PickledQuote, args: Unpickler.PickledArgs): TypeTree


  def Constraints_context[T]: scala.quoted.QuoteContext
  def Constraints_add(syms: List[Symbol]): Boolean
  def Constraints_approximation(sym: Symbol, fromBelow: Boolean): Type

  //
  // TYPES
  //

  /** Symbol of scala.internal.CompileTime.patternHole */
  def Definitions_InternalQuotedMatcher_patternHole: Symbol

  /** Symbol of scala.internal.CompileTime.higherOrderHole */
  def Definitions_InternalQuotedMatcher_higherOrderHole: Symbol

  /** Symbol of scala.internal.CompileTime.patternType */
  def Definitions_InternalQuotedMatcher_patternTypeAnnot: Symbol

  /** Symbol of scala.internal.CompileTime.fromAbove */
  def Definitions_InternalQuotedMatcher_fromAboveAnnot: Symbol

  /** Returns Some with a beta-reduced application or None */
  def betaReduce(tree: Term): Option[Term]

  def lambdaExtractor(term: Term, paramTypes: List[Type]): Option[List[Term] => Term]

  def compilerId: Int

}


object CompilerInterface {

  private[scala] def quoteContextWithCompilerInterface(qctx: QuoteContext): qctx.type { val tasty: qctx.tasty.type & scala.internal.tasty.CompilerInterface } =
    qctx.asInstanceOf[qctx.type { val tasty: qctx.tasty.type & scala.internal.tasty.CompilerInterface }]

}
