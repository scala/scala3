package dotty.tools.benchmarks.scripts

import java.time.ZonedDateTime
import com.github.tototoshi.csv.{CSVWriter, CSVFormat, DefaultCSVFormat}
import upickle.default.read

/** Aggregated results for one run of a benchmark at a specific commit. */
case class AggregatedRow(
    benchmark: String,
    benchTime: ZonedDateTime,
    commit: String,
    pr: Int,
    min: Double,
    q1: Double,
    median: Double,
    q3: Double,
    max: Double
)

/** Computes aggregated data from benchmark results in `dataCsv` and writes it
  * to the `output` directory.
  *
  * The input CSV file is expected to contain [[Results]] rows: one row per
  * benchmark result.
  *
  * **The output directory is cleared before writing the aggregated data.**
  *
  * Afterwards, two CSV files are written for each benchmark:
  *   - `all/[benchmark].csv`: all aggregated rows,
  *   - `last100/[benchmark].csv`: the last 100 aggregated rows.
  */
@main def aggregateData(dataDirString: String): Unit =
  val dataDir = parsePath(dataDirString)
  assert(os.exists(dataDir), s"`$dataDir` not found.")

  val rawData = dataDir / "raw"
  val aggregatedData = dataDir / "aggregated"

  os.remove.all(aggregatedData)
  os.makeDir.all(aggregatedData / "all")
  os.makeDir.all(aggregatedData / "last100")

  val rows =
    for
      file <- os.walk(rawData)
      if file.ext == "json"
      res = read[Results](os.read(file))
      if res.merged
      benchRes <- res.benchResults
    yield
      val sorted = benchRes.measures.sorted
      assert(sorted.nonEmpty, s"Empty measures for benchmark `${benchRes.benchmark}`.")
      AggregatedRow(
        benchRes.benchmark,
        res.benchTime,
        res.commit,
        res.pr,
        sorted.head,
        sorted.percentile(0.25),
        sorted.percentile(0.5),
        sorted.percentile(0.75),
        sorted.last
      )

  for (benchmark, benchmarkRows) <- rows.groupBy(_.benchmark) do
    val sortedBenchmarkRows = benchmarkRows.sortBy(_.benchTime)
    writeAggregatedRows(sortedBenchmarkRows, aggregatedData / "all" / s"$benchmark.csv")
    writeAggregatedRows(sortedBenchmarkRows.takeRight(100), aggregatedData / "last100" / s"$benchmark.csv")

/** Writes a sequence of [[AggregatedRow]]s in CSV at the given `path`. */
def writeAggregatedRows(rows: collection.Seq[AggregatedRow], path: os.Path) =
  given CSVFormat = new DefaultCSVFormat:
    override val lineTerminator = "\n"

  val writer = CSVWriter.open(path.toString())
  for row <- rows do
    writer.writeRow(Seq(row.benchTime.toString(), row.commit, row.pr, row.min, row.q1, row.median, row.q3, row.max))
  writer.close()

  println(s"Wrote ${rows.length} rows to $path.")
