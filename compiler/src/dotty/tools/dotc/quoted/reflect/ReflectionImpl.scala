package dotty.tools.dotc.quoted
package reflect

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core._
import dotty.tools.dotc.core.Contexts._

import scala.quoted.show.SyntaxHighlight

object ReflectionImpl {

  def apply(rootContext: Contexts.Context): scala.tasty.Reflection =
    new ReflectionImpl(rootContext)

  def showTree(tree: tpd.Tree)(using Contexts.Context): String = {
    val refl = new ReflectionImpl(MacroExpansion.context(tree))
    val reflCtx = ctx.asInstanceOf[refl.Context]
    val reflTree = tree.asInstanceOf[refl.Tree]
    val syntaxHighlight =
      if (ctx.settings.color.value == "always") SyntaxHighlight.ANSI
      else SyntaxHighlight.plain
    new scala.tasty.reflect.SourceCodePrinter[refl.type](refl)(syntaxHighlight).showTree(reflTree)(using reflCtx)
  }
}

// NOTE: This class should only mixin the compiler interface and the reflection interface.
//       We should not implement methods here, all should be implemented by `ReflectionCompilerInterface`
class ReflectionImpl(ctx: Context) extends ReflectionCompilerInterface(ctx) with scala.tasty.Reflection
