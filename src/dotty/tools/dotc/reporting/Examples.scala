package dotty.tools
package dotc
package reporting

import dotc.core.Contexts.Context
import dotc.printing.SyntaxHighlighting._

object ErrorExplanations {
  import dotc.ast.Trees._
  import dotc.ast.untpd

  implicit def stringToExplanation(s: String) = NoExplanation(s)

  implicit class ShouldExplainCtx(val c: Context) extends AnyVal {
    def shouldExplain(expl: Explanation): Boolean = {
      implicit val ctx = c
      expl match {
        case _: NoExplanation => false
        case expl if ctx.settings.explainerrors.value => true
        case _ => false
      }
    }
  }

  trait Explanation {
    def msg: String
    def explanation: String
  }

  case class NoExplanation(msg: String) extends Explanation {
    val explanation = ""
  }

  abstract class EmptyCatchOrFinallyBlock(tryBody: untpd.Tree)(implicit ctx: Context) extends Explanation {
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

  case class EmptyCatchBlock(tryBody: untpd.Tree)(implicit ctx: Context) extends EmptyCatchOrFinallyBlock(tryBody) {
    val msg =
      hl"""The ${"catch"} block does not contain a valid expression, try adding a case like - `${"case e: Exception =>"}` to the block"""
  }

  case class EmptyCatchAndFinallyBlock(tryBody: untpd.Tree)(implicit ctx: Context) extends EmptyCatchOrFinallyBlock(tryBody) {
    val msg =
      hl"""A ${"try"} without ${"catch"} or ${"finally"} is equivalent to putting its body in a block; no exceptions are handled."""
  }
}
