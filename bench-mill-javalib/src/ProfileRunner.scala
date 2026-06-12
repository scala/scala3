package dotty.tools.benchmarks.profile

import java.io.File
import java.nio.file.{Files, Paths, Path}
import scala.io.Source

import dotty.tools.dotc.{Driver, Compiler, report}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.reporting.{Reporter, StoreReporter, ConsoleReporter}
import dotty.tools.io.AbstractFile
import dotty.tools.FatalError

/** Mirrors MillJavalibBenchmark.SilentDriver but ALSO calls run.printSummary()
 *  so `-Vprofile` / `-Ystats` output is emitted. We skip Driver.finish() because
 *  it recurses on suspendedUnits and surfaces spurious cyclic errors after
 *  plugin macros run on this corpus.
 */
class SilentDriver extends Driver:
  override def doCompile(compiler: Compiler, files: List[AbstractFile])(using ctx: Context): Reporter =
    if files.isEmpty then ctx.reporter
    else
      try
        val run = compiler.newRun
        run.compile(files)
        run.printSummary()
        ctx.reporter
      catch
        case ex: FatalError =>
          report.error(ex.getMessage)
          ctx.reporter

object ProfileRunner:
  private def lines(p: Path): List[String] =
    val src = Source.fromFile(p.toFile)
    try src.getLines().map(_.trim).filter(_.nonEmpty).toList
    finally src.close()

  private def wipe(dir: Path): Unit =
    val walk = Files.walk(dir)
    try
      val it = walk.iterator()
      val buf = scala.collection.mutable.ArrayBuffer.empty[Path]
      while it.hasNext do buf += it.next()
      buf.reverseIterator.foreach(p => try Files.deleteIfExists(p) catch case _: Throwable => ())
    finally walk.close()

  def main(rawArgs: Array[String]): Unit =
    val runs = sys.env.getOrElse("PROFILE_RUNS", "3").toInt
    val benchDir = sys.env.get("BENCH_DIR") match
      case Some(d) => Paths.get(d)
      case None    => sys.error("BENCH_DIR env var is required (set by run_profile_runner)")
    val inputs = benchDir.resolve("inputs")
    val sources = benchDir.resolve("sources")
    val cp = lines(inputs.resolve("compile-classpath.txt")).mkString(File.pathSeparator)
    val opts = lines(inputs.resolve("scalac-options.txt"))
    val srcRel = lines(inputs.resolve("source-files.txt"))
    val sourceFiles = srcRel.map(r => sources.resolve(r).toAbsolutePath.toString).toArray
    val baseArgs = (List("-classpath", cp) ++ opts ++ rawArgs).toArray

    val verbosePrefixes = List("-Vprofile", "-Ystats", "-Vphases", "-Vprint", "-verbose")
    val verbose = rawArgs.exists(a => verbosePrefixes.exists(a.startsWith))
    var hadErrors = false

    for i <- 1 to runs do
      val outDir = Files.createTempDirectory(s"scala3-profile-out-$i-")
      val args = baseArgs ++ Array("-d", outDir.toAbsolutePath.toString) ++ sourceFiles
      val t0 = System.nanoTime()
      val reporter: Reporter = if verbose then ConsoleReporter() else StoreReporter(null)
      val driver = new SilentDriver
      val result = driver.process(args, reporter)
      val ms = (System.nanoTime() - t0) / 1_000_000L
      println(s"[profile-runner] run $i/$runs: $ms ms, errors=${result.errorCount}")
      if result.errorCount > 0 then
        hadErrors = true
        result.allErrors.take(5).foreach: d =>
          println(s"[profile-runner]   ERR: ${d.position.orElse(null)}: ${d.message.linesIterator.take(2).mkString(" / ")}")
      try wipe(outDir) catch case _: Throwable => ()
    if hadErrors then sys.exit(1)
