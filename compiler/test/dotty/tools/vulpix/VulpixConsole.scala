package dotty.tools.vulpix

import dotty.Properties
import java.util.concurrent.atomic.AtomicBoolean

private[vulpix] object VulpixConsole:
  final case class ActiveTest(title: String, runningSeconds: Long)
  final case class TestTiming(group: String, title: String, durationNanos: Long)

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
  private val startAnnounced = AtomicBoolean()

  def colorsEnabled(environment: collection.Map[String, String], isCI: Boolean): Boolean =
    isCI && environment.get("GITHUB_ACTIONS").contains("true") && !environment.contains("NO_COLOR")

  def colorsEnabled: Boolean = colorsEnabled(sys.env, Properties.isRunByCI)

  def pulseEnabled(environment: collection.Map[String, String], isCI: Boolean): Boolean =
    isCI && environment.get("VULPIX_CI_PULSE").contains("true")

  def pulseEnabled: Boolean = pulseEnabled(sys.env, Properties.isRunByCI)

  def startMarker(environment: collection.Map[String, String], isCI: Boolean): Option[String] =
    if !isCI then None
    else
      environment
        .get("VULPIX_CI_START_MARKER")
        .filter(marker => marker.nonEmpty && !marker.exists(Character.isISOControl))

  /** Tell the CI log wrapper that the first Vulpix batch is about to start. */
  def announceStart(): Unit =
    startMarker(sys.env, Properties.isRunByCI).foreach { marker =>
      if startAnnounced.compareAndSet(false, true) then
        System.out.println(marker)
        System.out.flush()
    }

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
    val failureColor = if progress.failed == 0 then Green else Red

    s"${styled("[Vulpix]", BoldCyan, useColors)} ${styled(groupText, Cyan, useColors)}" +
      s" | ${styled(s"${progress.completed}/${progress.total} complete", Cyan, useColors)}" +
      s" | ${styled(activeText, Cyan, useColors)}" +
      s" | ${styled(s"${progress.failed} failed in group", failureColor, useColors)}" +
      s" | ${styled(s"${formatDuration(progress.elapsedSeconds)} elapsed", Dim, useColors)}" +
      styled(longestText, Dim, useColors)

  def renderSlowestByGroup(timings: Iterable[TestTiming], useColors: Boolean): String =
    timings
      .groupBy(_.group)
      .toList
      .sortBy((group, _) => (sanitize(group, Int.MaxValue), group))
      .map { (group, groupTimings) =>
        renderSlowest(
          s"[Vulpix] Top 5 slowest in ${sanitize(group, 48)}:",
          groupTimings,
          includeGroup = false,
          useColors,
        )
      }
      .filter(_.nonEmpty)
      .mkString("\n")

  def renderSlowestOverall(timings: Iterable[TestTiming], useColors: Boolean): String =
    renderSlowest("[Vulpix] Top 5 slowest overall:", timings, includeGroup = true, useColors)

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

  private def renderSlowest(
    heading: String,
    timings: Iterable[TestTiming],
    includeGroup: Boolean,
    useColors: Boolean,
  ): String =
    val distinctTimings = timings.groupMapReduce(timing => (timing.group, timing.title))(timing => timing) {
      (left, right) => if left.durationNanos >= right.durationNanos then left else right
    }
    val slowest = distinctTimings.values.toList.sortWith(timingComesBefore).take(5)
    if slowest.isEmpty then ""
    else
      val rows = slowest.zipWithIndex.map { (timing, index) =>
        val group = if includeGroup then s"[${sanitize(timing.group, 48)}] " else ""
        val title = sanitize(timing.title, 120)
        val duration = styled(formatNanos(timing.durationNanos), Dim, useColors)
        s"  ${index + 1}. $group$title ($duration)"
      }
      (styled(heading, BoldCyan, useColors) :: rows).mkString("\n")

  private def timingComesBefore(left: TestTiming, right: TestTiming): Boolean =
    if left.durationNanos != right.durationNanos then left.durationNanos > right.durationNanos
    else
      val byGroup = left.group.compareTo(right.group)
      if byGroup != 0 then byGroup < 0
      else left.title.compareTo(right.title) < 0

  private def formatNanos(nanos: Long): String =
    val millis = math.max(nanos, 0L) / 1_000_000L
    if millis < 1_000 then s"${millis}ms"
    else
      val hours = millis / 3_600_000
      val minutes = millis % 3_600_000 / 60_000
      val seconds = millis % 60_000 / 1_000
      val remainder = millis % 1_000
      if hours > 0 then f"${hours}h${minutes}%02dm${seconds}%02d.${remainder}%03ds"
      else if minutes > 0 then f"${minutes}m${seconds}%02d.${remainder}%03ds"
      else f"${seconds}.${remainder}%03ds"

  private def formatDuration(seconds: Long): String =
    val safeSeconds = math.max(seconds, 0L)
    val hours = safeSeconds / 3600
    val minutes = safeSeconds % 3600 / 60
    val remainder = safeSeconds % 60
    if hours > 0 then f"${hours}h${minutes}%02dm"
    else if minutes > 0 then f"${minutes}m${remainder}%02ds"
    else s"${remainder}s"
