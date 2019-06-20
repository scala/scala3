package scala

package quoted {

  import scala.quoted.show.SyntaxHighlight

  sealed abstract class Expr[+T] {

    /** Evaluate the contents of this expression and return the result.
     *
     *  May throw a FreeVariableError on expressions that came from a macro.
     */
    @deprecated("Use scala.quoted.run", "")
    final def run(implicit toolbox: Toolbox): T = toolbox.run(_ => this)

  }

  object Expr {

    import scala.internal.quoted._

    implicit class ExprOps[T](expr: Expr[T]) {

      /** Show a source code like representation of this expression */
      def show(implicit qctx: QuoteContext): String = qctx.show(expr, SyntaxHighlight.plain)

      /** Show a source code like representation of this expression */
      def show(syntaxHighlight: SyntaxHighlight)(implicit qctx: QuoteContext): String = qctx.show(expr, syntaxHighlight)

    }

    /** Converts a tuple `(T1, ..., Tn)` to `(Expr[T1], ..., Expr[Tn])` */
    type TupleOfExpr[Tup <: Tuple] = Tuple.Map[Tup, Expr]

    implicit class AsFunction[F, Args <: Tuple, R](f: Expr[F]) given (tf: TupledFunction[F, Args => R]) {
      /** Beta-reduces the function appication. Generates the an expression only containing the body of the function */
      def apply[G] given (tg: TupledFunction[G, TupleOfExpr[Args] => Expr[R]]): G =
        tg.untupled(args => new FunctionAppliedTo[R](f, args.toArray.map(_.asInstanceOf[Expr[_]])))
    }

    implicit class AsContextualFunction[F, Args <: Tuple, R](f: Expr[F]) given (tf: TupledFunction[F, given Args => R]) {
      /** Beta-reduces the function appication. Generates the an expression only containing the body of the function */
      def apply[G] given (tg: TupledFunction[G, TupleOfExpr[Args] => Expr[R]]): G =
        tg.untupled(args => new FunctionAppliedTo[R](f, args.toArray.map(_.asInstanceOf[Expr[_]])))
    }

    /** Returns A expression containing a block with the given statements and ending with the expresion
     *  Given list of statements `s1 :: s2 :: ... :: Nil` and an expression `e` the resulting expression
     *  will be equivalent to `'{ $s1; $s2; ...; $e }`.
     */
    def block[T](statements: List[Expr[_]], expr: Expr[T])(implicit refl: tasty.Reflection): Expr[T] = {
      import refl._
      Block(statements.map(_.unseal), expr.unseal).seal.asInstanceOf[Expr[T]]
    }

  }

}

package internal {
  package quoted {

    import scala.quoted._

    /** An Expr backed by a pickled TASTY tree */
    final class TastyExpr[+T](val tasty: scala.runtime.quoted.Unpickler.Pickled, val args: Seq[Any]) extends Expr[T] {
      override def toString: String = s"Expr(<pickled tasty>)"
    }

    /** An Expr backed by a lifted value.
     *  Values can only be of type Boolean, Byte, Short, Char, Int, Long, Float, Double, Unit, String or Null.
     */
    final class LiftedExpr[+T](val value: T) extends Expr[T] {
      override def toString: String = s"Expr($value)"
    }

    /** An Expr backed by a tree. Only the current compiler trees are allowed.
     *
     *  These expressions are used for arguments of macros. They contain and actual tree
     *  from the program that is being expanded by the macro.
     *
     *  May contain references to code defined outside this TastyTreeExpr instance.
     */
    final class TastyTreeExpr[Tree](val tree: Tree) extends Expr[Any] {
      override def toString: String = s"Expr(<tasty tree>)"
    }

    // TODO Use a List in FunctionAppliedTo(val f: Expr[_], val args: List[Expr[_]])
    // FIXME: Having the List in the code above trigers an assertion error while testing dotty.tools.dotc.reporting.ErrorMessagesTests.i3187
    //        This test does redefine `scala.collection`. Further investigation is needed.
    /** An Expr representing `'{($f).apply($x1, ..., $xn)}` but it is beta-reduced when the closure is known */
    final class FunctionAppliedTo[+R](val f: Expr[_], val args: Array[Expr[_]]) extends Expr[R] {
      override def toString: String = s"Expr($f <applied to> ${args.toList})"
    }

  }
}
