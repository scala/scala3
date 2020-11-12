package scala.quoted

/** Quotation context provided by a macro expansion or in the scope of `scala.quoted.run`.
 *  Used to perform all operations on quoted `Expr` or `Type`.
 *
 *  It contains the low-level Typed AST API metaprogramming API.
 *  This API does not have the static type guarantiees that `Expr` and `Type` provide.
 *
 *  @param tasty Typed AST API. Usage: `def f(qctx: QuoteContext) = { import qctx.reflect._; ... }`.
 */
trait QuoteContext { self: internal.QuoteUnpickler & internal.QuoteMatching =>

  // Extension methods for `Expr[T]`
  extension [T](self: Expr[T]):
    /** Show a source code like representation of this expression without syntax highlight */
    def show: String

    /** Shows the tree as fully typed source code colored with ANSI */
    def showAnsiColored: String

    /** Pattern matches `this` against `that`. Effectively performing a deep equality check.
    *  It does the equivalent of
    *  ```
    *  this match
    *    case '{...} => true // where the contents of the pattern are the contents of `that`
    *    case _ => false
    *  ```
    */
    def matches(that: Expr[Any]): Boolean

    /** Return the unlifted value of this expression.
     *
     *  Returns `None` if the expression does not contain a value or contains side effects.
     *  Otherwise returns the `Some` of the value.
     */
    def unlift(using unlift: Unliftable[T]): Option[T] =
      unlift.fromExpr(self)(using QuoteContext.this)

    /** Return the unlifted value of this expression.
     *
     *  Emits an error and throws if the expression does not contain a value or contains side effects.
     *  Otherwise returns the value.
     */
    def unliftOrError(using unlift: Unliftable[T]): T =
      def reportError =
        val msg = s"Expected a known value. \n\nThe value of: ${self.show}\ncould not be unlifted using $unlift"
        report.throwError(msg, self)(using QuoteContext.this)
      unlift.fromExpr(self)(using QuoteContext.this).getOrElse(reportError)

    /** View this expression `quoted.Expr[T]` as a `Term` */
    def unseal: reflect.Term = self.asReflectTree // TODO remove

    /** View this expression `quoted.Expr[T]` as a `Term` */
    def asReflectTree: reflect.Term
  end extension

  // Extension methods for `Expr[Any]` that take another explicit type parameter
  extension [X](self: Expr[Any]):
    /** Checks is the `quoted.Expr[?]` is valid expression of type `X` */
    def isExprOf(using tp: scala.quoted.Type[X]): Boolean

    /** Convert this to an `quoted.Expr[X]` if this expression is a valid expression of type `X` or throws */
    def asExprOf(using tp: scala.quoted.Type[X]): scala.quoted.Expr[X]
  end extension

  /** Low-level Typed AST API metaprogramming API.
   *  This API does not have the static type guarantiees that `Expr` and `Type` provide.
   */
  val reflect: scala.quoted.Reflection
  // TODO move Reflcetion definition in here

  /** Type of a QuoteContext provided by a splice within a quote that took this context.
   *  It is only required if working with the reflection API.
   *
   *  Usually it is infered by the quotes an splices typing. But sometimes it is necessary
   *  to explicitly state that a context is nested as in the following example:
   *
   *  ```scala
   *  def run(using qctx: QuoteContext)(tree: qctx.reflect.Tree): Unit =
   *    def nested()(using qctx.Nested): Expr[Int] = '{  ${ makeExpr(tree) } + 1  }
   *    '{  ${ nested() } + 2 }
   *  def makeExpr(using qctx: QuoteContext)(tree: qctx.reflect.Tree): Expr[Int] = ???
   *  ```
   */
  type Nested = QuoteContext {
    val reflect: self.reflect.type
  }

}
