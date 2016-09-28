package dotty.tools
package dotc
package reporting
package diagnostic

import dotc.core._
import Contexts.Context, Decorators._, Symbols._, Names._, Types._
import util.{SourceFile, NoSource}
import util.{SourcePosition, NoSourcePosition}
import config.Settings.Setting
import interfaces.Diagnostic.{ERROR, WARNING, INFO}
import printing.SyntaxHighlighting._
import printing.Formatting

object messages {

  // `MessageContainer`s to be consumed by `Reporter` ---------------------- //
  class Error(
    msgFn: => Message,
    pos: SourcePosition,
    explanation: String = ""
  ) extends MessageContainer(msgFn, pos, ERROR, explanation)

  class Warning(
    msgFn: => Message,
    pos: SourcePosition,
    explanation: String = ""
  ) extends MessageContainer(msgFn, pos, WARNING, explanation)

  class Info(
    msgFn: => Message,
    pos: SourcePosition,
    explanation: String = ""
  ) extends MessageContainer(msgFn, pos, INFO, explanation)

  abstract class ConditionalWarning(
    msgFn: => Message,
    pos: SourcePosition,
    explanation: String = ""
  ) extends Warning(msgFn, pos, explanation) {
    def enablingOption(implicit ctx: Context): Setting[Boolean]
  }

  class FeatureWarning(
    msgFn: => Message,
    pos: SourcePosition,
    explanation: String = ""
  ) extends ConditionalWarning(msgFn, pos, explanation) {
    def enablingOption(implicit ctx: Context) = ctx.settings.feature
  }

  class UncheckedWarning(
    msgFn: => Message,
    pos: SourcePosition,
    explanation: String = ""
  ) extends ConditionalWarning(msgFn, pos, explanation) {
    def enablingOption(implicit ctx: Context) = ctx.settings.unchecked
  }

  class DeprecationWarning(
    msgFn: => Message,
    pos: SourcePosition,
    explanation: String = ""
  ) extends ConditionalWarning(msgFn, pos, explanation) {
    def enablingOption(implicit ctx: Context) = ctx.settings.deprecation
  }

  class MigrationWarning(
    msgFn: => Message,
    pos: SourcePosition,
    explanation: String = ""
  ) extends ConditionalWarning(msgFn, pos, explanation) {
    def enablingOption(implicit ctx: Context) = ctx.settings.migration
  }

  /**  Messages
    *  ========
    *  The role of messages is to provide the necessary details for a simple to
    *  understand diagnostic event. Each message can be turned into a message
    *  container (one of the above) by calling the appropriate method on them.
    *  For instance:
    *
    *  ```scala
    *  EmptyCatchBlock(tree).error(pos)   // res: Error
    *  EmptyCatchBlock(tree).warning(pos) // res: Warning
    *  ```
    */
  import dotc.ast.Trees._
  import dotc.ast.untpd

  // Syntax Errors ---------------------------------------------------------- //
  abstract class EmptyCatchOrFinallyBlock(tryBody: untpd.Tree, errNo: Int)(implicit ctx: Context)
  extends Message(errNo) {
    val explanation = {
      val tryString = tryBody match {
        case Block(Nil, untpd.EmptyTree) => "{}"
        case _ => tryBody.show
      }

      val code1 =
        s"""|try $tryString catch {
            |  case t: Throwable => ???
            |}""".stripMargin

      val code2 =
        s"""|try $tryString finally {
            |  // perform your cleanup here!
            |}""".stripMargin

      hl"""|A ${"try"} expression should be followed by some mechanism to handle any exceptions
           |thrown. Typically a ${"catch"} expression follows the ${"try"} and pattern matches
           |on any expected exceptions. For example:
           |
           |$code1
           |
           |It is also possible to follow a ${"try"} immediately by a ${"finally"} - letting the
           |exception propagate - but still allowing for some clean up in ${"finally"}:
           |
           |$code2""".stripMargin
    }
  }

  case class EmptyCatchBlock(tryBody: untpd.Tree)(implicit ctx: Context)
  extends EmptyCatchOrFinallyBlock(tryBody, 1) {
    val kind = "Syntax"
    val msg =
      hl"""|The ${"catch"} block does not contain a valid expression, try
           |adding a case like - `${"case e: Exception =>"}` to the block""".stripMargin
  }

  case class EmptyCatchAndFinallyBlock(tryBody: untpd.Tree)(implicit ctx: Context)
  extends EmptyCatchOrFinallyBlock(tryBody, 2) {
    val kind = "Syntax"
    val msg =
      hl"""|A ${"try"} without ${"catch"} or ${"finally"} is equivalent to putting
           |its body in a block; no exceptions are handled.""".stripMargin
  }

  case class DeprecatedWithOperator()(implicit ctx: Context)
  extends Message(3) {
    val kind = "Syntax"
    val msg =
      hl"""${"with"} as a type operator has been deprecated; use `&' instead"""
    val explanation = {
      val codeBlock1 =
        """|trait A {
           |  type T = Int
           |}
           |
           |trait B {
           |  type T = Double
           |}""".stripMargin

      hl"""|Dotty introduces intersection types - `&' types. These replace the
           |use of the ${"with"} keyword. There are a few differences in
           |semantics between intersection types and using `${"with"}'.
           |
           |`${"A with B"}' is ordered, `${"A & B"}' is not.
           |
           |In:
           |
           |$codeBlock1
           |
           |The type of `${"T"}' in `${"A with B"}' is ${"Int"} whereas in `${"A & B"}'
           |the type of `${"T"}' is ${"Int & Double"}.""".stripMargin
    }
  }

  // Type Errors ------------------------------------------------------------ //
  case class DuplicateBind(bind: untpd.Bind, tree: untpd.CaseDef)(implicit ctx: Context)
  extends Message(4) {
    val kind = "Naming"
    val msg = em"duplicate pattern variable: `${bind.name}`"

    val explanation = {
      val pat = tree.pat.show
      val guard = tree.guard match {
        case untpd.EmptyTree => ""
        case guard => s"if ${guard.show}"
      }

      val body = tree.body match {
        case Block(Nil, untpd.EmptyTree) => ""
        case body => s" ${body.show}"
      }

      val caseDef = s"case $pat$guard => $body"

      hl"""|For each ${"case"} bound variable names  have to be unique. In:
           |
           |$caseDef
           |
           |`${bind.name}` is not unique. Rename one of the bound variables!""".stripMargin
    }
  }

  case class MissingIdent(tree: untpd.Ident, treeKind: String, name: String)(implicit ctx: Context)
  extends Message(5) {
    val kind = "Missing Identifier"
    val msg = em"not found: $treeKind$name"

    val explanation = {
      hl"""|An identifier for `$treeKind$name` is missing. This means that something
           |has either been misspelt or you're forgetting an import""".stripMargin
    }
  }

  case class TypeMismatch(found: Type, expected: Type, whyNoMatch: String = "", implicitFailure: String = "")(implicit ctx: Context)
  extends Message(6) {
    val kind = "Type Mismatch"
    private val (where, printCtx) = Formatting.disambiguateTypes(found, expected)
    private val (fnd, exp) = Formatting.typeDiff(found, expected)(printCtx)
    val msg =
      s"""|found:    $fnd
          |required: $exp
          |
          |$where""".stripMargin + whyNoMatch + implicitFailure

    val explanation = ""
  }
}
