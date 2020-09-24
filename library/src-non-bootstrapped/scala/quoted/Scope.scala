package scala.quoted

import scala.internal.quoted.Unpickler

trait Trees {
  type Term
  type TypeTree // TODO get rid of one of them

  // used to construct trees from quotes
  def unpickleExpr(repr: Unpickler.PickledQuote, args: Unpickler.PickledArgs): Term
  def unpickleType(repr: Unpickler.PickledQuote, args: Unpickler.PickledArgs): TypeTree

  // used to match quoted patterns
  type Matching = Option[Tuple]
  def termMatch(scrutinee: Term, pattern: Term, hasTypeSplices: Boolean): Matching
  def typeMatch(scrutinee: TypeTree, pattern: Term, hasTypeSplices: Boolean): Matching
}


trait Quotes { self =>

  /** Low-level Typed AST API `tasty` meta-programming API.
   *  This API does not have the static type guarantiees that `Expr` and `Type` provide.
   */
  val tasty: Trees
}

trait Scope extends Quotes { self =>

  type Expr[+T]
  type Type[T <: AnyKind]

  val tasty: scala.tasty.Reflection

  type Nested = Scope {
    val tasty: self.tasty.type
  }
}