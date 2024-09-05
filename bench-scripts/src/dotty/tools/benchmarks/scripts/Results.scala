package dotty.tools.benchmarks.scripts

import java.time.{ZonedDateTime, ZoneOffset}
import java.time.format.DateTimeFormatter

import upickle.default.{readwriter, ReadWriter}

/** Results for one run of a benchmark */
case class BenchResults(
  benchmark: String,
  warmup: Seq[Double],
  measures: Seq[Double]
) derives ReadWriter

/** ReadWriter for ZonedDateTime in ISO format */
given dateReaderWriter: ReadWriter[ZonedDateTime] =
  readwriter[String].bimap[ZonedDateTime](
    _.format(DateTimeFormatter.ISO_INSTANT),
    ZonedDateTime.parse
  )

/** Raw results for one run of a benchmark at a specific commit.
  *
  * @param benchmark
  *   Name of the benchmark
  * @param commitTime
  *   Time of the benchmarked commit
  * @param commit
  *   Hash of the benchmarked commit
  * @param merged
  *   Whether the benchmarked commit was merged to `main`
  * @param pr
  *   Pull request corresponding to the benchmarked commit
  * @param benchTime
  *   Time at which the benchmark was run
  * @param run
  *   Run number
  * @param warmup
  *   Warmup times
  * @param measures
  *   Measurement times
  */
case class Results(
    commitTime: ZonedDateTime,
    commit: String,
    merged: Boolean,
    pr: Int,
    benchTime: ZonedDateTime,
    run: Int,
    benchResults: Seq[BenchResults]
) derives ReadWriter:
  assert(commitTime.getZone() == ZoneOffset.UTC, s"expected commit time '$commitTime' to be in UTC")
  assert(benchTime.getZone() == ZoneOffset.UTC, s"expected benchmark time '$benchTime' to be in UTC")
  assert(benchTime.isAfter(commitTime), s"expected benchmark time '$benchTime' to be after commit time '$commitTime'")
