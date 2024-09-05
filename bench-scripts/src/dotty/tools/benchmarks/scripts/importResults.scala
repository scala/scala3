package dotty.tools.benchmarks.scripts

import scala.sys.process.stringToProcess
import java.time.{ZonedDateTime, ZoneOffset}
import collection.mutable.ArrayBuffer
import upickle.default.write

/** Imports benchmark results from a JMH output file into a JSON file. */
@main def importResults(
    pr: Int,
    commit: String,
    merged: Boolean,
    run: Int,
    jmhOutputString: String,
    dataJsonString: String
): Unit =
  val jmhOutput = parsePath(jmhOutputString)
  assert(os.exists(jmhOutput), s"`$jmhOutput` not found.")
  val dataJson = parsePath(dataJsonString)
  assert(!os.exists(dataJson), s"`$dataJson` already exists.")
  val actualCommit = "git rev-parse HEAD".!!.trim
  assert(commit == actualCommit, "commit does not match HEAD")
  val commitTime = parseDate("git show -s --format=%cI".!!.trim)
  val benchTime = ZonedDateTime.now().withZoneSameInstant(ZoneOffset.UTC).withNano(0)
  importResults(pr, commit, merged, run, jmhOutput, dataJson, commitTime, benchTime)

def importResults(
    pr: Int,
    commit: String,
    merged: Boolean,
    run: Int,
    jmhOutput: os.ReadablePath,
    dataJson: os.Path,
    commitTime: ZonedDateTime,
    benchTime: ZonedDateTime
): Unit =
  println("Importing benchmark results...")
  println(s"pwd: ${os.pwd}")
  println(s"pr: $pr")
  println(s"commitTime: $commitTime")
  println(s"commit: $commit")
  println(s"benchTime: $benchTime")
  println(s"jmhOutput: $jmhOutput")
  println(s"dataJson: $dataJson")

  os.makeDir.all(dataJson / os.up)
  val results = Results(commitTime, commit, merged, pr, benchTime, run, readJMHResults(jmhOutput))
  os.write(dataJson, write(results, 2))
  println(s"Wrote results to $dataJson.")

/** Reads results from a JMH text output file. */
def readJMHResults(jmhOutput: os.ReadablePath): Seq[BenchResults] =
  val benchmarkPrefix = "# Benchmark: "
  val warmupPrefix = "# Warmup Iteration"
  val measurePrefix = "Iteration "
  val lines = os.read.lines(jmhOutput)
  val results = ArrayBuffer.empty[BenchResults]
  var benchmark = ""
  val warmup = ArrayBuffer.empty[Double]
  val measures = ArrayBuffer.empty[Double]
  for line <- lines do
    if line.startsWith(benchmarkPrefix) then
      if benchmark.nonEmpty then
        results += BenchResults(benchmark, warmup.toSeq, measures.toSeq)
        warmup.clear()
        measures.clear()
      benchmark = parseBenchmarkName(readValue(line))
    if line.startsWith(warmupPrefix) then
      warmup += parseTime(readValue(line))
    if line.startsWith(measurePrefix) then
      measures += parseTime(readValue(line))
  results += BenchResults(benchmark, warmup.toSeq, measures.toSeq)
  results.toSeq

/** Reads the value of a line that has the format `key: value`. */
def readValue(line: String): String =
  val parts = line.split(":")
  assert(parts.length == 2, s"expected 2 parts separated by ':' in line '$line'")
  parts(1).trim

/** Parses a benchmark method name into a short name. */
def parseBenchmarkName(methodName: String): String =
  val nightlySuffix = "Nightly"
  val name = methodName.split("\\.").last
  if name.endsWith(nightlySuffix) then name.dropRight(nightlySuffix.length) else name

/** Parses a time value from a JMH output line. It must end with 'ms/op'. */
def parseTime(time: String): Double =
  val timeUnit = " ms/op"
  assert(time.endsWith(timeUnit), s"expected $time to end with time unit '$timeUnit'")
  time.dropRight(timeUnit.length).toDouble
