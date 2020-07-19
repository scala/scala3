package dottyBench.tools.dotc.tastyreflect

import dottyBench.tools.dotc.ast.tpd
import dottyBench.tools.dotc.core._
import dottyBench.tools.dotc.core.Contexts._

import scala.quoted.show.SyntaxHighlight

object ReflectionImpl {

  def apply(rootContext: Contexts.Context): scala.tasty.Reflection =
    new scala.tasty.Reflection(new ReflectionCompilerInterface(rootContext))

  def showTree(tree: tpd.Tree)(using Contexts.Context): String = {
    val refl = new scala.tasty.Reflection(new ReflectionCompilerInterface(MacroExpansion.context(tree)))
    val reflCtx = ctx.asInstanceOf[refl.Context]
    val reflTree = tree.asInstanceOf[refl.Tree]
    val syntaxHighlight =
      if (ctx.settings.color.value == "always") SyntaxHighlight.ANSI
      else SyntaxHighlight.plain
    new scala.tasty.reflect.SourceCodePrinter[refl.type](refl)(syntaxHighlight).showTree(reflTree)(using reflCtx)
  }
}

