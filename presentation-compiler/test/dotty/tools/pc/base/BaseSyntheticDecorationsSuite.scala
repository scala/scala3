package dotty.tools.pc.base

import java.net.URI

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.metals.CompilerSyntheticDecorationsParams
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.language.unsafeNulls

import dotty.tools.pc.utils.TextEdits

import org.eclipse.lsp4j.TextEdit

class BaseSyntheticDecorationsSuite extends BasePCSuite {

  def check(
      base: String,
      expected: String,
      kind: Option[Int] = None,
  ): Unit =
      def pkgWrap(text: String) =
        if (text.contains("package")) text
        else s"package test\n$text"

      val withPkg = pkgWrap(base)
      val vFile = CompilerVirtualFileParams(
        URI.create("file:/Decorations.scala"),
        withPkg,
      )

      val pcParams = CompilerSyntheticDecorationsParams(
        vFile,
        true,
        true,
        true,
        true,
      )

      val allDecorations = presentationCompiler
        .syntheticDecorations(
          pcParams
        )
        .get()
        .asScala
        .toList

      val decorations = kind match {
        case Some(k) => allDecorations.filter(_.kind == k)
        case _ => allDecorations
      }

      val edits = decorations.map(d => new TextEdit(d.range(), d.label()))
      val obtained = TextEdits.applyEdits(withPkg, edits)

      assertNoDiff(
        obtained,
        pkgWrap(expected),
      )

}