package dotty.tools.pc.completions

import scala.meta.internal.mtags.CoursierComplete

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.pc.utils.InteractiveEnrichments.*

class ScalaCliCompletions(
    coursierComplete: CoursierComplete,
    pos: SourcePosition,
    text: String
):
  def unapply(path: List[Tree]) =
    def scalaCliDep = CoursierComplete.isScalaCliDep(
      pos.lineContent.take(pos.column).stripPrefix("/*<script>*/").dropWhile(c => c == ' ' || c == '\t')
    )

    lazy val supportsUsing =
      val filename = pos.source.file.path
      filename.endsWith(".sc.scala") ||
      filename.endsWith(".worksheet.sc")

    path match
      case Nil | (_: PackageDef) :: _ => scalaCliDep
      // generated script file will end with .sc.scala
      case (_: TypeDef) :: (_: PackageDef) :: Nil if supportsUsing =>
        scalaCliDep
      case (_: Template) :: (_: TypeDef) :: Nil if supportsUsing =>
        scalaCliDep
      case head :: next => None

  def contribute(dependency: String) =
    val completions = coursierComplete.complete(dependency)
    val (editStart, editEnd) = CoursierComplete.inferEditRange(pos.point, text)
    val editRange = pos.withStart(editStart).withEnd(editEnd).toLsp
    val normalized = dependency.replace(":::", ":").replace("::", ":")
    val isVersionCompletion = normalized.split(":").length >= 3
    completions
      .map(insertText =>
        CompletionValue.Coursier(
          insertText.stripPrefix(":"),
          Some(insertText),
          Some(editRange),
          isVersionCompletion
        )
      )

end ScalaCliCompletions
