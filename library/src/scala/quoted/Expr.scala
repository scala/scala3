package scala

package quoted {

  import scala.quoted.show.SyntaxHighlight

  sealed trait Expr[+T] {

    /** Show a source code like representation of this expression without syntax highlight */
    def show(implicit qctx: QuoteContext): String = qctx.show(this, SyntaxHighlight.plain)

    /** Show a source code like representation of this expression */
    def show(syntaxHighlight: SyntaxHighlight)(implicit qctx: QuoteContext): String = qctx.show(this, syntaxHighlight)

    /** Return the value of this expression.
     *
     *  Returns `None` if the expression does not contain a value or contains side effects.
     *  Otherwise returns the `Some` of the value.
     */
    final def getValue[U >: T] given (qctx: QuoteContext, valueOf: ValueOfExpr[U]): Option[U] = valueOf(this)

  }

  object Expr {

    import scala.internal.quoted._

    /** Converts a tuple `(T1, ..., Tn)` to `(Expr[T1], ..., Expr[Tn])` */
    type TupleOfExpr[Tup <: Tuple] = Tuple.Map[Tup, [X] =>> given QuoteContext => Expr[X]]

    implicit class AsFunction[F, Args <: Tuple, R](f: Expr[F]) given (tf: TupledFunction[F, Args => R], qctx: QuoteContext) {
      /** Beta-reduces the function appication. Generates the an expression only containing the body of the function */
      def apply[G] given (tg: TupledFunction[G, TupleOfExpr[Args] => Expr[R]]): G = {
        import qctx.tasty._
        tg.untupled(args => qctx.tasty.internal.betaReduce(f.unseal, args.toArray.toList.map(_.asInstanceOf[QuoteContext => Expr[_]](qctx).unseal)).seal.asInstanceOf[Expr[R]])
      }
    }

    implicit class AsContextualFunction[F, Args <: Tuple, R](f: Expr[F]) given (tf: TupledFunction[F, given Args => R], qctx: QuoteContext) {
      /** Beta-reduces the function appication. Generates the an expression only containing the body of the function */
      def apply[G] given (tg: TupledFunction[G, TupleOfExpr[Args] => Expr[R]]): G = {
        import qctx.tasty._
        tg.untupled(args => qctx.tasty.internal.betaReduce(f.unseal, args.toArray.toList.map(_.asInstanceOf[QuoteContext => Expr[_]](qctx).unseal)).seal.asInstanceOf[Expr[R]])
      }
    }

    /** Returns a null expresssion equivalent to `'{null}` */
    def nullExpr: given QuoteContext => Expr[Null] = given qctx => {
      import qctx.tasty._
      Literal(Constant(null)).seal.asInstanceOf[Expr[Null]]
    }

    /** Returns a unit expresssion equivalent to `'{}` or `'{()}` */
    def unitExpr: given QuoteContext => Expr[Unit] = given qctx => {
      import qctx.tasty._
      Literal(Constant(())).seal.asInstanceOf[Expr[Unit]]
    }

    /** Returns an expression containing a block with the given statements and ending with the expresion
     *  Given list of statements `s1 :: s2 :: ... :: Nil` and an expression `e` the resulting expression
     *  will be equivalent to `'{ $s1; $s2; ...; $e }`.
     */
    def block[T](statements: List[Expr[_]], expr: Expr[T])(implicit qctx: QuoteContext): Expr[T] = {
      import qctx.tasty._
      Block(statements.map(_.unseal), expr.unseal).seal.asInstanceOf[Expr[T]]
    }

  }

}

package internal {
  package quoted {

    import scala.quoted.Expr

    /** An Expr backed by a tree. Only the current compiler trees are allowed.
     *
     *  These expressions are used for arguments of macros. They contain and actual tree
     *  from the program that is being expanded by the macro.
     *
     *  May contain references to code defined outside this TastyTreeExpr instance.
     */
    final class TastyTreeExpr[Tree](val tree: Tree, val scopeId: Int) extends Expr[Any] {
      override def toString: String = s"Expr(<tasty tree>)"
    }

  }
}
