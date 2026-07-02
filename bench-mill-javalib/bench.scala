//| moduleDeps: [Common.scala]
//| scalaVersion: 3.8.2

import upickle.default.*

case class BenchResult(name: String, runs: Int, jvmExtra: String,
                       dotcExtra: Seq[String], timesMs: Seq[Int]) derives ReadWriter

def main(
    name: String = "",
    runs: Int = 20,
    dropFirst: Int = 10,
    jvmExtra: String = "",
    skipBuild: Boolean = false,
    analyzeOnly: Option[String] = None,
    forceSetup: Boolean = false,
    millVersion: String = MillVersion,
    dotc: mainargs.Leftover[String] = mainargs.Leftover(),
): Unit =
  if analyzeOnly.isEmpty && (forceSetup || !isSetupComplete) then runSetup(millVersion)
  val df = math.max(0, dropFirst)

  analyzeOnly match
    case Some(path) =>
      val data = read[BenchResult](os.read(os.Path(path, os.pwd)))
      println(s"[bench] analyzing $path  (name=${data.name}, runs=${data.timesMs.size})")
      report(data.timesMs, df)
    case None =>
      if name.isEmpty then sys.error("--name is required unless --analyze-only is given")
      val jvmOpts = Seq(
        "-Xms4g", "-Xmx4g",
        "-XX:+AlwaysPreTouch", "-XX:+UseG1GC", "-XX:MaxGCPauseMillis=200",
        "-XX:-UseAdaptiveSizePolicy", "-XX:CICompilerCount=4",
        "-Djava.awt.headless=true",
      ) ++ (if jvmExtra.isEmpty then Nil else jvmExtra.split("\\s+").toSeq)
      println(s"[bench] $name: runs=$runs  jvm='$jvmExtra'  dotc='${dotc.value.mkString(" ")}'")

      val timings = scala.collection.mutable.ListBuffer.empty[Int]
      val t0 = System.nanoTime()
      val rc = runProfileRunner(runs, jvmOpts, dotc.value, compilerClasspath(skipBuild),
        onLine = { line =>
          println(line)
          for m <- TimingRegex.findFirstMatchIn(line) do timings += m.group(1).toInt
        })
      val elapsed = (System.nanoTime() - t0) / 1_000_000_000L
      if rc != 0 then sys.error(s"[bench] $name: FAILED rc=$rc elapsed=${elapsed}s")
      if timings.isEmpty then sys.error("[bench] no per-run timings parsed from runner output")

      os.makeDir.all(BuildDir)
      val outJson = BuildDir / s"bench-$name.json"
      os.write.over(outJson, write(BenchResult(name, runs, jvmExtra, dotc.value, timings.toSeq), indent = 2) + "\n")
      println(s"[bench] $name: elapsed=${elapsed}s  times -> $outJson\n")
      report(timings.toSeq, df)

private def report(times: Seq[Int], dropFirst: Int): Unit =
  println("  iters: " + times.zipWithIndex.map((v, i) => f"$v%7d${if i < dropFirst then "*" else ""}").mkString(" | ")
    + "    (* = dropped from tail)")
  summarize("all iters       ", times)
  if 0 < dropFirst && dropFirst < times.size then
    summarize(f"drop-first-$dropFirst%-3d   ", times.drop(dropFirst))

private def summarize(label: String, times: Seq[Int]): Unit =
  if times.isEmpty then println(s"  $label: N=0")
  else
    val xs = times.map(_.toDouble)
    val n = xs.size
    val mean = xs.sum / n
    val sd = if n > 1 then math.sqrt(xs.iterator.map(v => (v - mean) * (v - mean)).sum / (n - 1)) else 0.0
    val median = xs.sorted.apply(n / 2)
    val rsd = if mean > 0 then 100 * sd / mean else 0.0
    println(f"  $label: N=$n  mean=$mean%.1fms  median=$median%.1fms  sd=$sd%.1fms  RSD=$rsd%.2f%%  min=${xs.min}%.0f  max=${xs.max}%.0f")
