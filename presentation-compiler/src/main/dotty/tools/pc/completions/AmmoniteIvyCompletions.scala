package dotty.tools.pc.completions

import scala.meta.internal.mtags.CoursierComplete

import dotty.tools.dotc.ast.untpd.ImportSelector
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.pc.utils.InteractiveEnrichments.*

object AmmoniteIvyCompletions:
  def contribute(
      coursierComplete: CoursierComplete,
      selector: List[ImportSelector],
      completionPos: CompletionPos,
      text: String
  )(using Context): List[CompletionValue] =
    val pos = completionPos.originalCursorPosition
    val query = selector.collectFirst {
      case sel: ImportSelector
          if sel.sourcePos.encloses(pos) && sel.sourcePos.`end` > pos.`end` =>
        sel.name.decoded.replace(Cursor.value, "")
    }
    query match
      case None => Nil
      case Some(dependency) =>
        val isInitialCompletion =
          pos.lineContent.trim() == "import $ivy."
        val ivyEditRange =
          if isInitialCompletion then completionPos.toEditRange
          else
            // We need the text edit to span the whole group/artefact/version
            val (rangeStart, rangeEnd) =
              CoursierComplete.inferEditRange(pos.point, text)
            pos.withStart(rangeStart).withEnd(rangeEnd).toLsp
        val completions = coursierComplete.complete(dependency.nn)
        val normalized = dependency.replace(":::", ":").replace("::", ":")
        val isVersionCompletion = normalized.split(":").length >= 3
        completions
          .map(insertText =>
            CompletionValue.Coursier(
              insertText.stripPrefix(":"),
              Some(insertText),
              Some(ivyEditRange),
              isVersionCompletion
            )
          )
