package dotty.tools.sbtplugin

import java.io.File

import sbt._

object Dist {
  def generateVersionFile(
    base: File,
    distDir: File,
    progVersion: String,
    log: String => Unit
  ): Unit = {
    import scala.util.Try
    import java.time.format.DateTimeFormatterBuilder
    import java.time.format.SignStyle
    import java.time.temporal.ChronoField.*
    import java.time.ZoneId
    import java.time.Instant
    import java.time.ZonedDateTime
    import java.time.ZonedDateTime
    import java.util.Locale
    import java.util.Date

    val base: File = new File(".") // Using the working directory as base for readability

    def write(path: String, content: String): Unit = {
      val p = distDir / path
      IO.write(p, content)
    }

    val humanReadableTimestampFormatter = new DateTimeFormatterBuilder()
        .parseCaseInsensitive()
        .appendValue(YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
        .appendLiteral('-')
        .appendValue(MONTH_OF_YEAR, 2)
        .appendLiteral('-')
        .appendValue(DAY_OF_MONTH, 2)
        .appendLiteral(' ')
        .appendValue(HOUR_OF_DAY, 2)
        .appendLiteral(':')
        .appendValue(MINUTE_OF_HOUR, 2)
        .appendLiteral(':')
        .appendValue(SECOND_OF_MINUTE, 2)
        .appendOffset("+HHMM", "Z")
        .toFormatter(Locale.US)

    // Retrieve build time
    val systemZone = ZoneId.systemDefault().normalized()
    val timestamp  = ZonedDateTime.ofInstant(Instant.ofEpochMilli(new Date().getTime), systemZone)
    val buildTime  = humanReadableTimestampFormatter.format(timestamp)

    // Check the current Git revision
    val gitRevision: String = Try {
      if ((base / ".git").exists()) {
        log("[republish] Checking the git revision of the current project")
        sys.process.Process("git rev-parse HEAD").!!
      } else {
        "unknown"
      }
    }.getOrElse("unknown").trim


    // Output the version number and Git revision
    write("VERSION", s"version:=${progVersion}\nrevision:=${gitRevision}\nbuildTime:=${buildTime}\n")
  }
}
