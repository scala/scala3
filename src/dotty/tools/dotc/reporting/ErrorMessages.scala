package dotty.tools
package dotc
package reporting

import dotc.core._
import Contexts.Context, Decorators._, Symbols._
import dotc.printing.SyntaxHighlighting._
import util.{SourcePosition, NoSourcePosition}

object ErrorMessages {
  import dotc.ast.Trees._
  import dotc.ast.untpd

  implicit class ShouldExplainCtx(val c: Context) extends AnyVal {
    def shouldExplain(expl: ErrorMessage): Boolean = {
      implicit val ctx = c
      expl match {
        case _: NoExplanation => false
        case expl if ctx.settings.explainerrors.value => true
        case _ => false
      }
    }
  }

  trait ErrorMessage {
    def kind: String
    def msg: String
    def explanation: String
  }

  case class NoExplanation(msg: String)(implicit val kind: String) extends ErrorMessage {
    val explanation = ""
  }

  object Syntax {
    implicit val kind: String = "Syntax"
    implicit def stringToErrorMessage(s: String): ErrorMessage = NoExplanation(s)

    abstract class EmptyCatchOrFinallyBlock(tryBody: untpd.Tree)(implicit ctx: Context, val kind: String) extends ErrorMessage {
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

        hl"""|Explanation:
             |============
             |A ${"try"} expression should be followed by some mechanism to handle any exceptions
             |thrown. Typically a ${"catch"} expression follows the ${"try"} and pattern matches
             |on any expected exceptions. For example:
             |
             |$code1
             |
             |It is also possible to follow a ${"try"} immediately by a ${"finally"} - letting the
             |exception propagate - but still allowing for some clean up in ${"finally"}:
             |
             |$code2
             """.stripMargin
      }
    }

    case class EmptyCatchBlock(tryBody: untpd.Tree)(implicit ctx: Context, override val kind: String)
    extends EmptyCatchOrFinallyBlock(tryBody) {
      val msg =
        hl"""The ${"catch"} block does not contain a valid expression, try adding a case like - `${"case e: Exception =>"}` to the block"""
    }

    case class EmptyCatchAndFinallyBlock(tryBody: untpd.Tree)(implicit ctx: Context, override val kind: String)
    extends EmptyCatchOrFinallyBlock(tryBody) {
      val msg =
        hl"""A ${"try"} without ${"catch"} or ${"finally"} is equivalent to putting its body in a block; no exceptions are handled."""
    }
  }

  object Type {
    implicit val kind: String = "Type"
    implicit def stringToErrorMessage(s: String): ErrorMessage = NoExplanation(s)

    case class DuplicateBind(bind: untpd.Bind, tree: untpd.CaseDef)(implicit ctx: Context, val kind: String) extends ErrorMessage {
      val msg =
        em"duplicate pattern variable: `${bind.name}`"

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

        hl"""|Explanation
             |===========
             |For each ${"case"} bound variable names  have to be unique. In:
             |
             |$caseDef
             |
             |`${bind.name}` is not unique. Rename one of the bound variables!""".stripMargin
      }
    }
  }
}
