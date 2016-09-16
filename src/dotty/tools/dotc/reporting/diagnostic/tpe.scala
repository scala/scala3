package dotty.tools
package dotc
package reporting
package diagnostic

import dotc.core._
import Contexts.Context, Decorators._, Symbols._, Names._
import dotc.printing.SyntaxHighlighting._
import util.{SourcePosition, NoSourcePosition}

object tpe {
  import dotc.ast.Trees._
  import dotc.ast.untpd

  class DuplicateBind(
    bind: untpd.Bind,
    tree: untpd.CaseDef
  )(implicit ctx: Context) extends MessageCreator {
    val kind = "Naming"

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

      hl"""|For each ${"case"} bound variable names  have to be unique. In:
           |
           |$caseDef
           |
           |`${bind.name}` is not unique. Rename one of the bound variables!""".stripMargin
    }
  }

  class MissingIdent(tree: untpd.Ident, treeKind: String, name: Name)(implicit ctx: Context) extends MessageCreator {
    val kind = "Missing identifier"
    val msg = em"not found: $treeKind$name"

    val explanation = {
      hl"""|An identifier for `${name.show}` is missing. This means that something
           |has either been misspelt or you're forgetting an import""".stripMargin
    }
  }
}
