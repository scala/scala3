//| moduleDeps: [Common.scala]
//| scalaVersion: 3.8.2

def main(
    repeat: Int = 5,
    runs: Int = 10,
    jfrOut: String = "",
    skipBuild: Boolean = false,
    analyzeOnly: Option[String] = None,
    thread: String = "main",
    newIter: Boolean = false,
    forceSetup: Boolean = false,
    millVersion: String = MillVersion,
): Unit =
  if analyzeOnly.isEmpty && (forceSetup || !isSetupComplete) then runSetup(millVersion)

  val resolvedOut: String =
    if jfrOut.nonEmpty then jfrOut
    else if analyzeOnly.isDefined then analyzeOnly.get
    else (nextRunDir(BenchDir, newIter = newIter) / "profile.jfr").toString

  val baseJfr = os.Path(resolvedOut, os.pwd)
  val stem = baseJfr.last.stripSuffix(".jfr")

  analyzeOnly match
    case Some(_) =>
      val all = for i <- 1 to repeat yield
        val jfr = if repeat > 1 then baseJfr / os.up / s"$stem-run$i.jfr" else baseJfr
        if !os.exists(jfr) then sys.error(s"JFR not found: $jfr")
        // Match recordAndAnalyze: build trees only from run 1.
        val (st, treesWritten) =
          if i == 1 then
            val full = analyzeJfrFull(jfr, thread)
            val treePath = baseJfr / os.up / s"$stem-trees.txt"
            writeTreeReport(full, treePath)
            (full.stats, Some(treePath))
          else (analyzeJfr(jfr, thread), None)
        val tail = treesWritten.fold("")(p => s"; trees -> ${p.last}")
        println(s"[profile] $jfr: ${st.kept} samples, ${st.nAllocEvents} alloc events, ${fmtBytes(st.totalBytes).trim} alloc$tail")
        st
      val summaryPath = baseJfr / os.up / s"$stem-summary.txt"
      writeSummary(all, summaryPath)
      println(s"[profile] summary saved to $summaryPath")
    case None =>
      val outDir = baseJfr / os.up
      os.makeDir.all(outDir)
      val before = os.list(outDir).toSet
      recordAndAnalyze(repeat, runs, baseJfr, compilerClasspath(skipBuild = skipBuild), thread)
      println(s"\n[profile] files in $outDir:")
      for p <- os.list(outDir).filter(os.isFile).sorted do
        println(s"${if !before.contains(p) then "  +" else "   "} $p")
