package dotty.tools.vulpix

import dotty.Properties
import java.util.concurrent.atomic.AtomicBoolean

private[vulpix] object VulpixConsole:
  final case class ActiveTest(title: String, runningSeconds: Long)
  final case class TestTiming(group: String, title: String, durationNanos: Long)

  final case class Progress(groupNames: List[String], completed: Int, total: Int, failed: Int,
    elapsedSeconds: Long, activeTests: List[ActiveTest])

  final case class Summary(passed: Int, failures: List[FailedTestInfo], skipped: Int)

  private val Dim = "\u001b[2m"
  private val BoldCyan = Console.BOLD + Console.CYAN
  private val startAnnounced = AtomicBoolean()

  def colorsEnabled(environment: collection.Map[String, String], isCI: Boolean): Boolean =
    isCI && environment.get("GITHUB_ACTIONS").contains("true") && !environment.contains("NO_COLOR")

  def colorsEnabled: Boolean = colorsEnabled(sys.env, Properties.isRunByCI)

  def pulseEnabled(environment: collection.Map[String, String], isCI: Boolean): Boolean =
    isCI && environment.get("RUNNER_DEBUG").contains("1")

  def pulseEnabled: Boolean = pulseEnabled(sys.env, Properties.isRunByCI)

  /** Close the CI section containing sbt startup when the first Vulpix batch starts. */
  def announceStart(): Unit =
    if Properties.isRunByCI && sys.env.get("VULPIX_CI_FOLD").contains("true")
        && startAnnounced.compareAndSet(false, true)
    then println("::endgroup::")

  def renderSummary(summary: Summary, useColors: Boolean): String =
    val failures = summary.failures
    val total = summary.passed + failures.size + summary.skipped
    val testLabel = if summary.passed == 1 then "test" else "tests"
    val passedText = styled(s"${summary.passed} $testLabel passed", Console.GREEN, useColors)
    val skippedText =
      if summary.skipped == 0 then ""
      else s", ${styled(s"${summary.skipped} skipped", Console.YELLOW, useColors)}"
    if failures.isEmpty then
      styled(s"== Vulpix Test Report: ${summary.passed} $testLabel passed, no failures$skippedText ==", Console.GREEN, useColors)
    else
      val failedLines = failures.map(info => styled(s"    ${info.title}${info.extra}", Console.RED, useColors)).mkString("\n")
      s"""|${styled("=" * 80, Console.CYAN, useColors)}
          |${styled("Vulpix Test Report", BoldCyan, useColors)}
          |${styled("=" * 80, Console.CYAN, useColors)}
          |
          |$passedText, ${styled(s"${failures.size} failed", Console.RED, useColors)}$skippedText, ${styled(s"$total total", Console.BOLD, useColors)}
          |${styled("Failed tests:", Console.RED, useColors)}
          |$failedLines
          |""".stripMargin

  def renderProgress(progress: Progress, useColors: Boolean): String =
    val groups = abbreviated(progress.groupNames.map(sanitize(_, 48)), 3)
    val groupText = if groups.isEmpty then "tests" else groups
    val activeText = s"${progress.activeTests.size} running"
    val longest = progress.activeTests.sortBy(_.runningSeconds)(using Ordering.Long.reverse)
    val longestText =
      if longest.isEmpty then ""
      else
        val shown = longest.take(2).map { active =>
          s"${sanitize(active.title, 72)} (${formatDuration(active.runningSeconds)})"
        }
        val hidden = longest.size - shown.size
        val suffix = if hidden == 0 then "" else s", +$hidden"
        s" | longest: ${shown.mkString(", ")}$suffix"
    val failureColor = if progress.failed == 0 then Console.GREEN else Console.RED

    s"${styled("[Vulpix]", BoldCyan, useColors)} ${styled(groupText, Console.CYAN, useColors)}" +
      s" | ${styled(s"${progress.completed}/${progress.total} complete", Console.CYAN, useColors)}" +
      s" | ${styled(activeText, Console.CYAN, useColors)}" +
      s" | ${styled(s"${progress.failed} failed in group", failureColor, useColors)}" +
      s" | ${styled(s"${formatDuration(progress.elapsedSeconds)} elapsed", Dim, useColors)}" +
      styled(longestText, Dim, useColors)

  def renderGroupStarts(groups: Iterable[String], useColors: Boolean): String =
    groups.toList.sorted
      .map(group =>
        s"${styled("[Vulpix]", BoldCyan, useColors)} Starting ${styled(sanitize(group, 48), Console.CYAN, useColors)}")
      .mkString("\n")

  def renderSlowestByGroup(timings: Iterable[TestTiming], useColors: Boolean): String =
    timings
      .groupBy(_.group)
      .toList
      .sortBy(_._1)
      .map((group, groupTimings) => renderSlowest(
        s"[Vulpix] Top 5 slowest in ${sanitize(group, 48)}:", groupTimings, includeGroup = false, useColors))
      .mkString("\n")

  def renderSlowestOverall(timings: Iterable[TestTiming], useColors: Boolean): String =
    renderSlowest("[Vulpix] Top 5 slowest overall:", timings, includeGroup = true, useColors)

  def stripColors(text: String): String =
    text.replaceAll("\u001b\\[.*?m", "")

  private def styled(text: String, code: String, enabled: Boolean): String =
    if enabled then s"$code$text${Console.RESET}" else text

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

  private def renderSlowest(heading: String, timings: Iterable[TestTiming], includeGroup: Boolean,
    useColors: Boolean): String =
    val distinctTimings = timings.groupBy(timing => (timing.group, timing.title)).values.map(_.maxBy(_.durationNanos))
    val slowest = distinctTimings.toList.sortBy(t => (-t.durationNanos, t.group, t.title)).take(5)
    if slowest.isEmpty then ""
    else
      val rows = slowest.zipWithIndex.map { (timing, index) =>
        val group = if includeGroup then s"[${sanitize(timing.group, 48)}] " else ""
        val title = sanitize(timing.title, 120)
        val duration = styled(formatNanos(timing.durationNanos), Dim, useColors)
        s"  ${index + 1}. $group$title ($duration)"
      }
      (styled(heading, BoldCyan, useColors) :: rows).mkString("\n")

  private def formatNanos(nanos: Long): String =
    formatDuration(math.max(nanos, 0L) / 1_000_000L, precise = true)

  private def formatDuration(seconds: Long): String =
    val safe = math.max(seconds, 0L)
    if safe < 60 then s"${safe}s" else formatDuration(safe * 1_000L, precise = false)

  private def formatDuration(millis: Long, precise: Boolean): String =
    if millis < 1_000 then s"${millis}ms"
    else
      val hours = millis / 3_600_000
      val minutes = millis % 3_600_000 / 60_000
      val seconds = millis % 60_000 / 1_000
      val fraction = if precise then f".${millis % 1_000}%03d" else ""
      if hours > 0 then f"${hours}h${minutes}%02dm${seconds}%02d${fraction}s"
      else if minutes > 0 then f"${minutes}m${seconds}%02d${fraction}s"
      else s"$seconds${fraction}s"
