/*
 * Migration rule for the `Option.orNull` API tightening that landed in
 * Scala 3.x (after explicit-nulls stabilization). The standard library no
 * longer accepts a type parameter on `Option#orNull` and the result type
 * is now `A | Null` instead of `A`.
 *
 * Mechanical rewrite, semantics-preserving:
 *
 *   opt.orNull[T]   -->   opt.getOrElse(null.asInstanceOf[T])
 *
 * The bare-call form (`opt.orNull`) is intentionally NOT rewritten because
 * `A | Null` is sometimes the actually-desired type. We emit a lint
 * diagnostic at those sites so the maintainer can decide.
 */
package fix

import scalafix.v1._
import scala.meta._

class OptionOrNullMigration extends SemanticRule("OptionOrNullMigration") {

  private val OptionOrNull = SymbolMatcher.normalized("scala/Option#orNull.")

  override def fix(implicit doc: SemanticDocument): Patch =
    doc.tree.collect {
      // opt.orNull[T] -> opt.getOrElse(null.asInstanceOf[T])
      case t @ Term.ApplyType.After_4_6_0(
            Term.Select(qual, fn @ Term.Name("orNull")),
            Type.ArgClause(List(tpe))
          ) if OptionOrNull.matches(fn) =>
        // Replace only the `orNull[T]` suffix to preserve `qual`'s original
        // whitespace/newlines. fn.pos.start is the start of the method name;
        // t.pos.end is the end of the type-arg clause.
        Patch.replaceToken(
          fn.tokens.head,
          "getOrElse"
        ) +
          Patch.removeTokens(fn.tokens.tail) +
          Patch.replaceToken(
            t.tokens.find(_.text == "[").get,
            "(null.asInstanceOf["
          ) +
          Patch.replaceToken(
            t.tokens.findLast(_.text == "]").get,
            "])"
          )

      // bare opt.orNull -> warn (semantics-changing rewrite skipped).
      // Skip when the call is wrapped by a type application — that case is
      // handled by the rule above.
      case sel @ Term.Select(_, fn @ Term.Name("orNull"))
          if OptionOrNull.matches(fn) &&
            !sel.parent.exists {
              case _: Term.ApplyType => true
              case _                 => false
            } =>
        Patch.lint(
          Diagnostic(
            id = "OrNullSemanticChange",
            message =
              "Option.orNull now returns A | Null instead of A. " +
                "If you needed A, replace with `.getOrElse(null.asInstanceOf[A])`.",
            position = fn.pos
          )
        )
    }.asPatch
}
