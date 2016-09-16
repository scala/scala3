package dotty.tools
package dotc
package reporting
package diagnostic

import dotc.core._
import Contexts.Context, Decorators._, Symbols._
import dotc.printing.SyntaxHighlighting._
import util.{SourcePosition, NoSourcePosition}

object syntax {
  import dotc.ast.Trees._
  import dotc.ast.untpd

  abstract class EmptyCatchOrFinallyBlock(tryBody: untpd.Tree)(implicit ctx: Context) extends MessageCreator {
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

  class EmptyCatchBlock(tryBody: untpd.Tree)(implicit ctx: Context)
  extends EmptyCatchOrFinallyBlock(tryBody) {
    val kind = "Syntax"
    val msg =
      hl"""|The ${"catch"} block does not contain a valid expression, try
           |adding a case like - `${"case e: Exception =>"}` to the block""".stripMargin
  }

  case class EmptyCatchAndFinallyBlock(tryBody: untpd.Tree)(implicit ctx: Context)
  extends EmptyCatchOrFinallyBlock(tryBody) {
    val kind = "Syntax"
    val msg =
      hl"""|A ${"try"} without ${"catch"} or ${"finally"} is equivalent to putting
           |its body in a block; no exceptions are handled.""".stripMargin
  }
}
