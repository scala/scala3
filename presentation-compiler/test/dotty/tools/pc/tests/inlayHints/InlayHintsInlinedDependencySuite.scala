package dotty.tools.pc.tests.inlayHints

import java.net.URI

import scala.jdk.CollectionConverters.*
import scala.language.unsafeNulls
import scala.meta.internal.metals.{CompilerInlayHintsParams, CompilerRangeParams}

import dotty.tools.pc.RawScalaPresentationCompiler
import dotty.tools.pc.base.TestResources
import dotty.tools.pc.utils.PcAssertions

import org.junit.Test

/** Regression test for a crash thrown while computing inlay hints over code
 *  that expands a `transparent inline` member from a *separately compiled*
 *  dependency (the real-world trigger was chimney's `into[...].transform`, but
 *  nothing chimney-specific is required).
 *
 *  A `transparent inline` expansion keeps its trees' positions pointing into
 *  the *defining* dependency's source (unlike a plain `inline def`, whose
 *  expansion is remapped to the call site). For a real classpath dependency
 *  that source is only available as TASTy: the line sizes are pickled (so
 *  `source.length` is non-zero) but `source.content()` is empty (the text is
 *  not pickled and the `.scala` file is not on disk). `XRayModeHint.isEndOfLine`
 *  then evaluates `end >= source.length || source(end) == '\n'`: the guard
 *  passes because `length` comes from the line sizes, and `source(end)` indexes
 *  the empty `content()` array -> ArrayIndexOutOfBoundsException.
 *
 *  The fixture lives in the `scala3-presentation-compiler-testcases` module,
 *  which is compiled with a `-sourceroot` that makes its `SOURCEFILEattr`
 *  unresolvable from the test working directory - so the presentation compiler
 *  sees it as TASTy-only, with empty `content()`, just like a distributed jar.
 *  See [[tests.inlayHints.into]].
 *
 *  IMPORTANT: this drives the *raw* presentation compiler on purpose. The
 *  default `ScalaPresentationCompiler.inlayHints` runs inside
 *  `withInterruptableCompiler(emptyDefault, ...)`, which catches the throwable
 *  and returns an empty list - hiding the crash. sls (and this test) use
 *  `RawScalaPresentationCompiler`, whose `inlayHints` calls `provide()`
 *  directly, so the exception propagates.
 */
class InlayHintsInlinedDependencySuite extends PcAssertions:

  private lazy val pc =
    RawScalaPresentationCompiler().newInstance(
      "",
      TestResources.classpath.asJava,
      List.empty[String].asJava
    )

  @Test def `inlay-hints-over-inlined-foreign-source`: Unit =
    val code =
      """|package test
         |
         |import tests.inlayHints.*
         |
         |object Main:
         |  val s: String = ???
         |  val w = into(s)
         |""".stripMargin

    val rangeParams =
      CompilerRangeParams(URI.create("file:/InlayHints.scala"), code, 0, code.length())
    val pcParams = CompilerInlayHintsParams(
      rangeParams = rangeParams,
      inferredTypes = true,
      typeParameters = true,
      implicitParameters = true,
      hintsXRayMode = true,
      byNameParameters = true,
      implicitConversions = true,
      namedParameters = true,
      hintsInPatternMatch = true,
      closingLabels = true
    )

    // Must not throw. Pre-fix this raises ArrayIndexOutOfBoundsException from
    // XRayModeHint.isEndOfLine on the fixture's TASTy-only (empty-content) source.
    pc.inlayHints(pcParams)
