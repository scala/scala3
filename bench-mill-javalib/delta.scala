//| moduleDeps: [Common.scala]
//| scalaVersion: 3.8.2

// Print before/after profile deltas for the given method names. Pair this with
// the go/no-go rule (max(sd_before, sd_after) < |delta|) when judging significance.

case class Row(method: String, selfMean: Double, selfStd: Double, totMean: Double, totStd: Double)

def parseSummary(path: os.Path): Vector[Row] =
  if !os.exists(path) then sys.error(s"not found: $path")
  val lines = os.read.lines(path)
  val start = lines.indexWhere(_.startsWith("--- Self / total time"))
  if start < 0 then sys.error(s"no '--- Self / total time' section in $path")
  // Skip the header line just after the section title.
  lines.iterator.drop(start + 2).takeWhile(l => !l.startsWith("---") && l.trim.nonEmpty)
    .flatMap { line =>
      val toks = line.trim.split("\\s+", 5)
      if toks.length == 5 then
        scala.util.Try(Row(toks(4), toks(0).toDouble, toks(1).toDouble, toks(2).toDouble, toks(3).toDouble)).toOption
      else None
    }.toVector

def resolveSummary(arg: String): os.Path =
  val p = os.Path(arg, os.pwd)
  if os.isDir(p) then
    val s = p / "profile-summary.txt"
    if os.exists(s) then s
    else sys.error(s"no profile-summary.txt in $p")
  else if os.isFile(p) then p
  else
    // Allow short forms relative to BenchDir: "iter-3/run-5", "run-5"
    // (resolves to the latest iter), etc.
    val direct = BenchDir / os.SubPath(arg) / "profile-summary.txt"
    if os.exists(direct) then return direct
    if arg.startsWith("run-") then
      val iters = os.list(BenchDir).filter(p => p.last.startsWith("iter-") && os.isDir(p))
      val latest = iters.maxByOption(_.last.stripPrefix("iter-").toIntOption.getOrElse(-1))
      latest.foreach { it =>
        val candidate = it / arg / "profile-summary.txt"
        if os.exists(candidate) then return candidate
      }
    sys.error(s"can't resolve $arg (tried $p, $direct, and latest iter)")

private def fmtChange(bM: Double, bS: Double, aM: Double, aS: Double): String =
  val d = aM - bM
  val n = math.max(bS, aS)
  val sigma = if n > 0 then math.abs(d) / n else 0.0
  f"$bM%5.2f ± $bS%4.2f → $aM%5.2f ± $aS%4.2f  ($d%+5.2f, $sigma%4.1fσ)"

def main(
    before: String,
    after: String,
    methods: mainargs.Leftover[String] = mainargs.Leftover(),
): Unit =
  val bPath = resolveSummary(before)
  val aPath = resolveSummary(after)
  val bRows = parseSummary(bPath)
  val aRows = parseSummary(aPath)
  val patterns = methods.value

  def matching(rows: Vector[Row], pat: String): Vector[Row] =
    rows.filter(_.method.contains(pat))

  val keys = patterns.flatMap(p => (matching(bRows, p) ++ matching(aRows, p)).map(_.method))
    .distinct.sorted

  if keys.isEmpty then
    println(s"no matches for ${patterns.mkString(", ")} in $bPath or $aPath")
    return

  println(s"before: $bPath")
  println(s"after:  $aPath")
  println()
  println("Go/no-go: a row is significant when |Δ| > max(sd_before, sd_after) (σ > 1.0).")
  for k <- keys do
    val b = bRows.find(_.method == k)
    val a = aRows.find(_.method == k)
    (b, a) match
      case (Some(br), Some(ar)) =>
        println(s"\n$k")
        println(s"  self%: ${fmtChange(br.selfMean, br.selfStd, ar.selfMean, ar.selfStd)}")
        println(s"  tot%:  ${fmtChange(br.totMean, br.totStd, ar.totMean, ar.totStd)}")
      case (Some(br), None) =>
        println(s"\n$k  (after: not in summary)")
        println(f"  self%%: ${br.selfMean}%5.2f ± ${br.selfStd}%4.2f → -")
        println(f"  tot%%:  ${br.totMean}%5.2f ± ${br.totStd}%4.2f → -")
      case (None, Some(ar)) =>
        println(s"\n$k  (before: not in summary)")
        println(f"  self%%: - → ${ar.selfMean}%5.2f ± ${ar.selfStd}%4.2f")
        println(f"  tot%%:  - → ${ar.totMean}%5.2f ± ${ar.totStd}%4.2f")
      case _ =>
