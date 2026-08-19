package dotty.tools.vulpix

import dotty.Properties

private[vulpix] object VulpixConsole:
  final case class ActiveTest(title: String, runningSeconds: Long)

  final case class Progress(
    groupNames: List[String],
    completed: Int,
    total: Int,
    failed: Int,
    elapsedSeconds: Long,
    activeTests: List[ActiveTest],
  )

  final case class Summary(passed: Int, failures: List[FailedTestInfo], skipped: Int)

  private val Reset = "\u001b[0m"
  private val Bold = "\u001b[1m"
  private val Dim = "\u001b[2m"
  private val Red = "\u001b[31m"
  private val Green = "\u001b[32m"
  private val Yellow = "\u001b[33m"
  private val Cyan = "\u001b[36m"
  private val BoldCyan = Bold + Cyan

  def colorsEnabled(environment: collection.Map[String, String], isCI: Boolean): Boolean =
    isCI && environment.get("GITHUB_ACTIONS").contains("true") && !environment.contains("NO_COLOR")

  def colorsEnabled: Boolean = colorsEnabled(sys.env, Properties.isRunByCI)

  def renderSummary(summary: Summary, useColors: Boolean): String =
    val failures = summary.failures
    val total = summary.passed + failures.size + summary.skipped
    val testLabel = if summary.passed == 1 then "test" else "tests"
    val passedText = styled(s"${summary.passed} $testLabel passed", Green, useColors)
    val failureColor = if failures.isEmpty then Green else Red
    val failedText = styled(s"${failures.size} failed", failureColor, useColors)
    val skippedText =
      if summary.skipped == 0 then ""
      else s", ${styled(s"${summary.skipped} skipped", Yellow, useColors)}"
    val failedLines =
      if failures.isEmpty then ""
      else
        failures
          .map(info => styled(s"    ${info.title}${info.extra}", Red, useColors))
          .mkString(s"${styled("Failed tests:", Red, useColors)}\n", "\n", "\n")

    s"""|${styled("=" * 80, Cyan, useColors)}
        |${styled("Vulpix Test Report", BoldCyan, useColors)}
        |${styled("=" * 80, Cyan, useColors)}
        |
        |$passedText, $failedText$skippedText, ${styled(s"$total total", Bold, useColors)}
        |$failedLines""".stripMargin

  def renderProgress(progress: Progress, useColors: Boolean): String =
    val groups = abbreviated(progress.groupNames.map(sanitize(_, 48)), 3)
    val groupText = if groups.isEmpty then "tests" else groups
    val activeText = s"${progress.activeTests.size} running"
    val longestText =
      if progress.activeTests.isEmpty then ""
      else
        val shown = progress.activeTests.take(2).map { active =>
          s"${sanitize(active.title, 72)} (${formatDuration(active.runningSeconds)})"
        }
        val hidden = progress.activeTests.size - shown.size
        val suffix = if hidden == 0 then "" else s", +$hidden"
        s" | longest: ${shown.mkString(", ")}$suffix"
    val failureColor = if progress.failed == 0 then Green else Red

    s"${styled("[Vulpix]", BoldCyan, useColors)} ${styled(groupText, Cyan, useColors)}" +
      s" | ${styled(s"${progress.completed}/${progress.total} complete", Cyan, useColors)}" +
      s" | ${styled(activeText, Cyan, useColors)}" +
      s" | ${styled(s"${progress.failed} failed in group", failureColor, useColors)}" +
      s" | ${styled(s"${formatDuration(progress.elapsedSeconds)} elapsed", Dim, useColors)}" +
      styled(longestText, Dim, useColors)

  def stripColors(text: String): String =
    text.replaceAll("\u001b\\[.*?m", "")

  private def styled(text: String, code: String, enabled: Boolean): String =
    if enabled then s"$code$text$Reset" else text

  private def abbreviated(values: List[String], limit: Int): String =
    val distinct = values.distinct
    val shown = distinct.take(limit)
    val hidden = distinct.size - shown.size
    if hidden == 0 then shown.mkString(", ")
    else shown.mkString(", ") + s", +$hidden"

  private def sanitize(text: String, maxLength: Int): String =
    val clean = text.iterator.map(ch => if Character.isISOControl(ch) then '?' else ch).mkString
    if clean.length <= maxLength then clean
    else "..." + clean.takeRight(maxLength - 3)

  private def formatDuration(seconds: Long): String =
    val safeSeconds = math.max(seconds, 0L)
    val hours = safeSeconds / 3600
    val minutes = safeSeconds % 3600 / 60
    val remainder = safeSeconds % 60
    if hours > 0 then f"${hours}h${minutes}%02dm"
    else if minutes > 0 then f"${minutes}m${remainder}%02ds"
    else s"${remainder}s"
