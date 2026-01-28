package dotty.tools.pc.base

import java.net.URI

import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.pc.CancelToken
import scala.meta.pc.VirtualFileParams

import dotty.tools.pc.RawScalaPresentationCompiler
import dotty.tools.pc.base.TestResources
import dotty.tools.pc.utils.PcAssertions
import dotty.tools.pc.utils.TestExtensions.getOffset

import org.eclipse.lsp4j.Diagnostic
import org.eclipse.lsp4j.DiagnosticSeverity

class BaseDiagnosticsSuite extends PcAssertions:
  case class TestDiagnostic(
      startIndex: Int,
      endIndex: Int,
      msg: String,
      severity: DiagnosticSeverity
  )

  def options: List[String] = Nil

  val pc = RawScalaPresentationCompiler().newInstance(
    "",
    TestResources.classpath.asJava,
    options.asJava
  )

  case class TestVirtualFileParams(uri: URI, text: String)
      extends VirtualFileParams:
    override def shouldReturnDiagnostics: Boolean = true
    override def token: CancelToken = EmptyCancelToken

  def check(
      text: String,
      expected: List[TestDiagnostic],
      additionalChecks: List[Diagnostic] => Unit = identity
  ): Unit =
    val diagnostics = pc
      .didChange(
        TestVirtualFileParams(URI.create("file:/Diagnostic.scala"), text)
      )
      .asScala

    val actual = diagnostics.map(d =>
      TestDiagnostic(
        d.getRange().getStart().getOffset(text),
        d.getRange().getEnd().getOffset(text),
        d.getMessage(),
        d.getSeverity()
      )
    )
    assertEquals(
      expected,
      actual,
      s"Expected [${expected.mkString(", ")}] but got [${actual.mkString(", ")}]"
    )
    additionalChecks(diagnostics.toList)
