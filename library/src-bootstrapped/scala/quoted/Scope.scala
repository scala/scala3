package scala.quoted

import scala.quoted.show.SyntaxHighlight

import scala.internal.quoted.Unpickler

/** Current Scope instance */
def scope(using s: Scope): s.type = s


/**
 * This is the minimal API used to construct quotes and match quoted patterns
 */
trait Trees {
  type Term
  type TypeTree // TODO it is sad that TypeTree is part of the small API

  // used to construct trees from quotes
  // TODO rename unpickleExpr to unpickleTerm
  def unpickleExpr(repr: Unpickler.PickledQuote, args: Unpickler.PickledArgs): Term
  def unpickleType(repr: Unpickler.PickledQuote, args: Unpickler.PickledArgs): TypeTree

  // used to match quoted patterns
  type Matching = Option[Tuple]
  def termMatch(scrutinee: Term, pattern: Term, hasTypeSplices: Boolean): Matching
  def typeMatch(scrutinee: TypeTree, pattern: TypeTree, hasTypeSplices: Boolean): Matching
}

trait Quotes { self =>

  type Expr[+T] <: tasty.Term

  type Type[T <: AnyKind] <: tasty.TypeTree


  /** Low-level Typed AST API `tasty` meta-programming API.
   *  This API does not have the static type guarantiees that `Expr` and `Type` provide.
   */
  val tasty: Trees

  /** Type of a Scope provided by a splice within a quote that took this context.
   *  It is only required if working with the reflection API.
   *
   *  Usually it is infered by the quotes an splices typing. But sometimes it is necessary
   *  to explicitly state that a context is nested as in the following example:
   *
   *  ```scala
   *  def run(using s: Scope)(tree: s.tasty.Tree): Unit =
   *    def nested()(using s2: s.Nested): s2.Expr[Int] = '{  ${ makeExpr(tree) } + 1  }
   *    '{  ${ nested() } + 2 }
   *  def makeExpr(using s: Scope)(tree: s.tasty.Tree): s.Expr[Int] = ???
   *  ```
   */
  type Nested <: Scope {
    type Expr[+T] >: self.Expr[T]
    type Type[T <: AnyKind] >: self.Type[T]
    val tasty: self.tasty.type
  }
}


/** Quotation context provided by a macro expansion or in the scope of `scala.quoted.run`.
 *  Used to perform all operations on quoted `Expr` or `Type`.
 *
 *  It contains the low-level Typed AST API `tasty` meta-programming API.
 *  This API does not have the static type guarantiees that `Expr` and `Type` provide.
 *
 *  @param tasty Typed AST API. Usage: `def f(using s: Scope) = { import s.tasty._; ... }`.
 */
trait Scope extends Quotes with Exprs, Types, Liftables, Unliftables { self =>

  /** Low-level Typed AST API `tasty` meta-programming API.
   *  This API does not have the static type guarantiees that `Expr` and `Type` provide.
   * 
   * TODO: Rename
   */
  val tasty: scala.tasty.Reflection
}



// tests/neg-macros/i4044b.scala failed
// tests/neg-macros/i9801 failed
// tests/neg-macros/quotedPatterns-1.scala failed
// tests/pos-macros/i8302.scala failed
// tests/pos-macros/i9812.scala failed
// tests/run-macros/i9812b failed
// tests/run-macros/refined-selectable-macro failed
// tests/run-macros/tasty-getfile-implicit-by-name-fun-context failed



// tests/run-staging/multi-staging.scala failed
// tests/run-staging/staged-streams_1.scala failed
// tests/run-staging/staged-tuples failed
