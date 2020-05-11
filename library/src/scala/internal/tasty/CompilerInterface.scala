package scala.internal.tasty

import scala.tasty.Reflection
import scala.tasty.reflect._
import scala.internal.quoted.Unpickler
import scala.quoted.Scope

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


  /////////////////
  // Constraints //
  /////////////////

  def Constraints_context[T]: scala.quoted.Scope
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

  /** The type of primitive type `Unit`. */
  def Definitions_UnitType: Type

  /** The type of primitive type `Byte`. */
  def Definitions_ByteType: Type

  /** The type of primitive type `Short`. */
  def Definitions_ShortType: Type

  /** The type of primitive type `Char`. */
  def Definitions_CharType: Type

  /** The type of primitive type `Int`. */
  def Definitions_IntType: Type

  /** The type of primitive type `Long`. */
  def Definitions_LongType: Type

  /** The type of primitive type `Float`. */
  def Definitions_FloatType: Type

  /** The type of primitive type `Double`. */
  def Definitions_DoubleType: Type

  /** The type of primitive type `Boolean`. */
  def Definitions_BooleanType: Type

  /** Returns Some with a beta-reduced application or None */
  def betaReduce(tree: Term): Option[Term]

  def lambdaExtractor(term: Term, paramTypes: List[Type]): Option[List[Term] => Term]

  def compilerId: Int

}


object CompilerInterface {

  private[scala] def quoteContextWithCompilerInterface(s: Scope): s.type { val tasty: s.tasty.type & scala.internal.tasty.CompilerInterface } =
    s.asInstanceOf[s.type { val tasty: s.tasty.type & scala.internal.tasty.CompilerInterface }]

}
