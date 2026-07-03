//| scalaVersion: 3.8.2

import jdk.jfr.consumer.{RecordingFile, RecordedClass, RecordedMethod, RecordedThread}
import java.nio.file.{FileAlreadyExistsException, Files}
import scala.collection.mutable
import scala.util.Using

/** Per-worktree paths. `scala3Dir` is where sbt builds the compiler from
 *  (the worktree whose diff is being profiled). `benchDir` is anchored to
 *  the *main* worktree (via `git rev-parse --git-common-dir`) so corpus
 *  and run output are unified across worktrees. */
class Env(val scala3Dir: os.Path):
  val benchScriptDir = scala3Dir / "bench-mill-javalib"
  val mainRepoDir    = Env.detectMainRepoRoot(scala3Dir)
  val benchDir       = mainRepoDir / "target" / "bench-mill-javalib"
  val inputsDir      = benchDir / "inputs"
  val sourcesDir     = benchDir / "sources"
  val buildDir       = benchDir / "target" / "build"
  val runnerSrc      = benchScriptDir / "src" / "ProfileRunner.scala"

object Env:
  given default: Env =
    val file = sourcecode.File()
    val scala3Dir =
      if java.nio.file.Paths.get(file).isAbsolute then os.Path(file) / os.up / os.up
      else os.pwd
    Env(scala3Dir)

  /** Locate the main worktree by following `git rev-parse --git-common-dir`.
   *  Falls back to `here` if not a git repo. */
  private[Env] def detectMainRepoRoot(here: os.Path): os.Path =
    scala.util.Try {
      val raw = os.proc("git", "rev-parse", "--git-common-dir").call(cwd = here).out.text().trim
      os.Path(raw, here) / os.up
    }.getOrElse(here)

// Top-level aliases for setup/profile/bench scripts that reference these in
// default-arg expressions. All resolve to Env.default.
export Env.default.{scala3Dir as Scala3Dir, benchScriptDir as BenchScriptDir,
                    benchDir as BenchDir, inputsDir as InputsDir,
                    sourcesDir as SourcesDir, buildDir as BuildDir,
                    runnerSrc as RunnerSrc}

/** Atomically claim the next `iter-N/run-M/` slot under `benchDir`. If `newIter` is
 *  false (default), extends the latest existing iter with a fresh run-M; if
 *  true (or no iter exists yet), starts a new iter-N and writes run-0. */
def nextRunDir(benchDir: os.Path, newIter: Boolean = false): os.Path =
  def listIdx(dir: os.Path, prefix: String): Seq[Int] =
    if !os.exists(dir) then Nil
    else os.list(dir).filter(os.isDir).iterator.flatMap { p =>
      val n = p.last
      if n.startsWith(prefix) then n.stripPrefix(prefix).toIntOption else None
    }.toSeq
  def tryClaim(dir: os.Path): Boolean =
    try
      Files.createDirectory(dir.toNIO)
      true
    catch case _: FileAlreadyExistsException => false
  def claimRun(iterDir: os.Path, startIdx: Int): os.Path =
    var runIdx = startIdx
    var claimed: Option[os.Path] = None
    while claimed.isEmpty do
      val runDir = iterDir / s"run-$runIdx"
      if tryClaim(runDir) then claimed = Some(runDir)
      runIdx += 1
    claimed.get
  def claimNewIter(startIdx: Int): os.Path =
    var iterIdx = startIdx
    var claimed: Option[os.Path] = None
    while claimed.isEmpty do
      val iterDir = benchDir / s"iter-$iterIdx"
      if tryClaim(iterDir) then claimed = Some(claimRun(iterDir, 0))
      iterIdx += 1
    claimed.get
  Files.createDirectories(benchDir.toNIO)
  val iters = listIdx(benchDir, "iter-")
  val useNew = newIter || iters.isEmpty
  if useNew then claimNewIter(iters.maxOption.map(_ + 1).getOrElse(0))
  else
    val iterDir = benchDir / s"iter-${iters.max}"
    val runIdx = listIdx(iterDir, "run-").maxOption.map(_ + 1).getOrElse(0)
    claimRun(iterDir, runIdx)

val ProfileRunnerClass = "dotty.tools.benchmarks.profile.ProfileRunner"
val TimingRegex = raw"\[profile-runner\] run \d+/\d+:\s+(\d+) ms".r

def sbtExport(target: String)(using env: Env): String =
  val r = os.call(("sbt", "--error", s"export $target"), cwd = env.scala3Dir, check = false)
  if r.exitCode != 0 then
    sys.error(s"sbt export $target failed (rc=${r.exitCode})\n${r.out.text()}\n${r.err.text()}")
  val cps = r.out.lines().filter(_.contains(":"))
  if cps.isEmpty then sys.error(s"sbt export $target produced no classpath:\n${r.out.text()}")
  cps.maxBy(_.length)

// Matches both `compiling 5 Scala sources` and `compiling 3 Java sources`,
// including when both appear on the same line (e.g. `compiling 5 Scala
// sources and 3 Java sources to ...`).
private val CompilingLine = raw"compiling (\d+) (?:Scala|Java) sources?".r

def compilerClasspath(skipBuild: Boolean = false)(using env: Env): String =
  if !skipBuild then
    var compiled = 0
    var sawCompile = false
    val rc = os.call(
      cmd = ("sbt", "scala3-compiler-nonbootstrapped/compile"),
      cwd = env.scala3Dir,
      stdout = os.ProcessOutput.Readlines { line =>
        println(line)
        for m <- CompilingLine.findAllMatchIn(line) do
          sawCompile = true
          compiled += m.group(1).toInt
      },
      mergeErrIntoOut = true, check = false,
    ).exitCode
    if rc != 0 then sys.error(s"sbt compile failed (rc=$rc)")
    if !sawCompile then sys.error(
      "sbt compile produced no `compiling N (Scala|Java) source(s)` line — " +
      "Zinc may be stale (no recompilation after `git checkout`?). Investigate before retrying."
    )
    println(s"[compile] sbt reported compiling $compiled source(s)")
  val cp = sbtExport("scala3-compiler-nonbootstrapped/Compile/fullClasspath")
    .split(java.io.File.pathSeparator)
    .iterator.filter(p => p.nonEmpty && os.exists(os.Path(p)))
    .mkString(java.io.File.pathSeparator)
  if cp.isEmpty then sys.error("empty compiler classpath")
  cp

/** Compile ProfileRunner.scala against the local non-bootstrapped compiler. */
def buildProfileRunner(compilerCp: String)(using env: Env): os.Path =
  val worktreeKey = java.lang.Integer.toHexString(env.scala3Dir.toString.hashCode)
  val classes = env.buildDir / "profile" / worktreeKey / "classes"
  os.remove.all(classes)
  os.makeDir.all(classes)
  os.call(
    cmd = ("java", "-cp", compilerCp, "dotty.tools.dotc.Main",
           "-classpath", compilerCp, "-d", classes, env.runnerSrc),
    stdout = os.Inherit, stderr = os.Inherit,
  )
  classes

/** Run ProfileRunner in a fresh JVM. `onLine` sees every output line; the
 *  default just prints. bench overrides it to also scrape per-iter timings. */
def runProfileRunner(runs: Int, jvmOpts: Seq[String], dotcExtra: Seq[String],
                     compilerCp: String, onLine: String => Unit = println)
                    (using env: Env): Int =
  val classes = buildProfileRunner(compilerCp)
  val cp = Seq(classes.toString, compilerCp).mkString(java.io.File.pathSeparator)
  os.call(
    cmd = ("java", jvmOpts, "-cp", cp, ProfileRunnerClass, dotcExtra),
    cwd = env.scala3Dir,
    env = Map("PROFILE_RUNS" -> runs.toString, "BENCH_DIR" -> env.benchDir.toString),
    stdout = os.ProcessOutput.Readlines(onLine),
    mergeErrIntoOut = true,
    check = false,
  ).exitCode

// JFR analysis via the binary API. Replaces `jfr print` + regex parsing.

case class RunStats(
    kept: Int,
    leafCounts: Map[String, Int],
    totalCounts: Map[String, Int],
    bytesByClass: Map[String, Long],
    totalBytes: Long,
    nAllocEvents: Int,
)

/** One ExecutionSample's stack as method-key strings, innermost first. */
case class JfrSample(methods: Array[String])

/** One allocation event: object class, attributed bytes, innermost-first stack. */
case class JfrAllocSample(objClass: String, sizeBytes: Long, methods: Array[String])

/** Result of a tree-building analysis: aggregated stats plus the raw per-event
 *  frame arrays needed to build top-down / bottom-up forests. */
case class JfrAnalysis(stats: RunStats, samples: Vector[JfrSample], allocs: Vector[JfrAllocSample])

def analyzeJfr(jfrPath: os.Path, threadName: String = "main"): RunStats =
  analyzeJfrImpl(jfrPath, threadName, collectFrames = false).stats

def analyzeJfrFull(jfrPath: os.Path, threadName: String = "main"): JfrAnalysis =
  analyzeJfrImpl(jfrPath, threadName, collectFrames = true)

private def analyzeJfrImpl(jfrPath: os.Path, threadName: String, collectFrames: Boolean): JfrAnalysis =
  val lc = mutable.HashMap.empty[String, Int]
  val tc = mutable.HashMap.empty[String, Int]
  val bc = mutable.HashMap.empty[String, Long]
  var kept, nAlloc = 0
  var totalBytes = 0L
  val seen = mutable.HashSet.empty[String]
  // Intern method-key strings: avoids ~millions of duplicate Strings when
  // collectFrames=true (30k samples * up to 256 frames each).
  val pool = mutable.HashMap.empty[String, String]
  def intern(s: String): String = pool.getOrElseUpdate(s, s)
  def methodKey(m: RecordedMethod): String = intern(s"${m.getType.getName}.${m.getName}")
  def bump[K](map: mutable.HashMap[K, Int], k: K): Unit = map(k) = map.getOrElse(k, 0) + 1
  def bumpL[K](map: mutable.HashMap[K, Long], k: K, n: Long): Unit = map(k) = map.getOrElse(k, 0L) + n
  val sampleBuf = if collectFrames then Vector.newBuilder[JfrSample] else null
  val allocBuf  = if collectFrames then Vector.newBuilder[JfrAllocSample] else null

  Using.resource(RecordingFile(jfrPath.toNIO)) { rf =>
    while rf.hasMoreEvents() do
      val ev = rf.readEvent()
      ev.getEventType.getName match
        case "jdk.ExecutionSample" =>
          // ExecutionSample's relevant thread is `sampledThread`, NOT
          // `eventThread` (which is JFR's internal sampler thread).
          val st = ev.getStackTrace
          val th = ev.getValue("sampledThread").asInstanceOf[RecordedThread | Null]
          if st != null && th != null && th.getJavaName == threadName then
            val frames = st.getFrames
            if !frames.isEmpty then
              kept += 1
              val methods = if collectFrames then new Array[String](frames.size) else null
              bump(lc, methodKey(frames.get(0).getMethod))
              seen.clear()
              var i = 0
              while i < frames.size do
                val k = methodKey(frames.get(i).getMethod)
                seen += k
                if collectFrames then methods(i) = k
                i += 1
              seen.foreach(bump(tc, _))
              if collectFrames then sampleBuf += JfrSample(methods)
        case "jdk.ObjectAllocationInNewTLAB" | "jdk.ObjectAllocationOutsideTLAB" =>
          val th = ev.getThread
          if th != null && th.getJavaName == threadName then
            ev.getValue("objectClass").asInstanceOf[RecordedClass | Null] match
              case null =>
              case cls =>
                val size = ev.getLong("allocationSize")
                val clsName = intern(cls.getName)
                bumpL(bc, clsName, size)
                totalBytes += size
                nAlloc += 1
                if collectFrames then
                  val st2 = ev.getStackTrace
                  if st2 != null then
                    val frames = st2.getFrames
                    if !frames.isEmpty then
                      val methods = new Array[String](frames.size)
                      var i = 0
                      while i < frames.size do
                        methods(i) = methodKey(frames.get(i).getMethod)
                        i += 1
                      allocBuf += JfrAllocSample(clsName, size, methods)
        case _ =>
  }
  val stats = RunStats(kept, lc.toMap, tc.toMap, bc.toMap, totalBytes, nAlloc)
  if collectFrames then JfrAnalysis(stats, sampleBuf.result(), allocBuf.result())
  else JfrAnalysis(stats, Vector.empty, Vector.empty)

def meanStd(xs: Seq[Double]): (Double, Double) =
  xs.size match
    case 0 => (0.0, 0.0)
    case 1 => (xs.head, 0.0)
    case n =>
      val m = xs.sum / n
      (m, math.sqrt(xs.iterator.map(v => (v - m) * (v - m)).sum / (n - 1)))

def fmtBytes(n: Long): String =
  val (gib, mib, kib) = (1L << 30, 1L << 20, 1L << 10)
  if n >= gib then f"${n.toDouble / gib}%6.2f GiB"
  else if n >= mib then f"${n.toDouble / mib}%6.2f MiB"
  else if n >= kib then f"${n.toDouble / kib}%6.2f KiB"
  else f"$n%6d B  "

def writeSummary(runs: Seq[RunStats], out: os.Path,
                 minPct: Double = 0.05, methodTop: Int = 200, allocTop: Int = 30): Unit =
  val n = runs.size
  val sb = StringBuilder()
  sb ++= s"=== Profile summary across $n runs ===\n"
  sb ++= s"  samples per run:      ${runs.map(_.kept).mkString(", ")}\n"
  sb ++= s"  alloc events per run: ${runs.map(_.nAllocEvents).mkString(", ")}\n"
  sb ++= s"  alloc bytes per run:  ${runs.map(r => fmtBytes(r.totalBytes).trim).mkString(", ")}\n"

  val methods = runs.iterator.flatMap(_.totalCounts.keysIterator).toSet
  val mrows = methods.iterator.flatMap { m =>
    val sp = runs.map(r => if r.kept == 0 then 0.0 else 100.0 * r.leafCounts.getOrElse(m, 0) / r.kept)
    val tp = runs.map(r => if r.kept == 0 then 0.0 else 100.0 * r.totalCounts.getOrElse(m, 0) / r.kept)
    val (ms, ss) = meanStd(sp); val (mt, st) = meanStd(tp)
    Option.when(ms >= minPct || mt >= minPct)((ms, ss, mt, st, m))
  }.toVector.sortBy { case (ms, _, mt, _, m) => (-ms, -mt, m) }.take(methodTop)

  sb ++= f"\n--- Self / total time (mean ± std across $n runs, top ${mrows.size}, floor either-mean >= $minPct%.2f%%) ---\n"
  sb ++= f"  ${"self%"}%7s ${"±std"}%6s   ${"tot%"}%7s ${"±std"}%6s   method\n"
  for (ms, ss, mt, st, m) <- mrows do
    sb ++= f"  $ms%7.2f $ss%6.2f   $mt%7.2f $st%6.2f   $m\n"

  if runs.exists(_.totalBytes > 0) then
    val classes = runs.iterator.flatMap(_.bytesByClass.keysIterator).toSet
    val arows = classes.iterator.flatMap { c =>
      val sh = runs.map(r => if r.totalBytes == 0 then 0.0 else 100.0 * r.bytesByClass.getOrElse(c, 0L) / r.totalBytes)
      val bv = runs.map(r => r.bytesByClass.getOrElse(c, 0L).toDouble)
      val (msh, ssh) = meanStd(sh); val (mb, std_b) = meanStd(bv)
      Option.when(msh >= minPct)((msh, ssh, mb, std_b, c))
    }.toVector.sortBy { case (msh, _, _, _, c) => (-msh, c) }.take(allocTop)

    sb ++= f"\n--- Allocation share / bytes (mean ± std across $n runs, top ${arows.size}, floor mean >= $minPct%.2f%%) ---\n"
    sb ++= f"  ${"share%"}%7s ${"±std"}%6s   ${"mean bytes"}%10s ${"±std bytes"}%10s   class\n"
    for (msh, ssh, mb, std_b, c) <- arows do
      sb ++= f"  $msh%7.2f $ssh%6.2f   ${fmtBytes(mb.toLong)} ${fmtBytes(std_b.toLong)}   $c\n"

  os.write.over(out, sb.toString, createFolders = true)

// ----- Tree construction (top-down + bottom-up + alloc forest) ---------
// Ported from the original Python profile.py (commit 398538f3, 2026-05-16),
// which was rewritten into the flat-table-only Scala port in 7be940a8 and
// lost the tree output in the process. We only build trees on the first
// JFR of a multi-run profile because trees are for eyeballing call shape,
// not for the mean ± std cross-run comparisons writeSummary produces.

class TreeNode(val name: String):
  var total: Long = 0L
  var selfCount: Long = 0L
  val children: mutable.HashMap[String, TreeNode] = mutable.HashMap.empty
  def child(n: String): TreeNode = children.getOrElseUpdate(n, TreeNode(n))

def buildTopDown(samples: Vector[JfrSample]): TreeNode =
  // Walk each stack outer-to-inner (reversed, since methods(0) is the leaf)
  // so the root holds entry-point frames and depth grows toward the leaf.
  val root = TreeNode("<root>")
  for s <- samples do
    if s.methods.nonEmpty then
      var cur = root
      cur.total += 1
      var i = s.methods.length - 1
      while i >= 0 do
        cur = cur.child(s.methods(i))
        cur.total += 1
        i -= 1
      cur.selfCount += 1
  root

def buildCallerTree(samples: Vector[JfrSample], leafMethod: String): TreeNode =
  // Root is the leaf method; children are immediate callers walking OUT.
  val root = TreeNode(leafMethod)
  for s <- samples do
    if s.methods.nonEmpty && s.methods(0) == leafMethod then
      root.total += 1
      root.selfCount += 1
      var cur = root
      var i = 1
      while i < s.methods.length do
        cur = cur.child(s.methods(i))
        cur.total += 1
        i += 1
  root

def buildAllocCallerTree(allocs: Vector[JfrAllocSample], objClass: String): TreeNode =
  // Bytes-weighted reverse forest: same shape as buildCallerTree but each
  // node's `total` is summed allocation bytes, not sample count. We start
  // from frame 0 (the allocation site) since the "leaf" is the class itself.
  val root = TreeNode(objClass)
  for s <- allocs do
    if s.objClass == objClass then
      root.total += s.sizeBytes
      root.selfCount += s.sizeBytes
      var cur = root
      var i = 0
      while i < s.methods.length do
        cur = cur.child(s.methods(i))
        cur.total += s.sizeBytes
        i += 1
  root

private def appendTopDown(sb: StringBuilder, root: TreeNode, kept: Int,
                          thresholdPct: Double, maxDepth: Int): Unit =
  if kept == 0 then return
  val threshold = thresholdPct * kept / 100.0
  sb ++= f"\n=== Top-down call tree (>= $thresholdPct%.2f%% of $kept samples) ===\n"
  sb ++= f"  ${"tot%"}%6s ${"self%"}%6s  call tree\n"
  def walk(node: TreeNode, depth: Int): Unit =
    if depth > maxDepth then return
    val kids = node.children.values.toArray.sortBy(k => -k.total)
    var i = 0
    while i < kids.length do
      val kid = kids(i)
      if kid.total >= threshold then
        val tot = 100.0 * kid.total / kept
        val slf = 100.0 * kid.selfCount / kept
        sb ++= f"  $tot%6.2f $slf%6.2f  ${"  " * depth}+ ${kid.name}\n"
        walk(kid, depth + 1)
      i += 1
  walk(root, 0)

private def appendCallerForest(sb: StringBuilder, samples: Vector[JfrSample],
                               leafCounts: Map[String, Int], kept: Int, topN: Int,
                               leafPctThreshold: Double, maxDepth: Int): Unit =
  if kept == 0 || topN <= 0 then return
  sb ++= f"\n=== Bottom-up reverse forest (top $topN self methods, caller >= $leafPctThreshold%.1f%% of leaf samples) ===\n"
  sb ++= "  Each `---` block has a leaf hotspot (marked `*`) followed by\n"
  sb ++= "  its caller chain (marked `^`, walking OUT toward main).\n"
  sb ++= "  Columns:\n"
  sb ++= "    tot%  = % of ALL profile samples whose stack contains\n"
  sb ++= "            this caller-chain ending at the leaf. The leaf's\n"
  sb ++= "            own row reports its self %.\n"
  sb ++= "    leaf% = % of the LEAF method's own samples that came in\n"
  sb ++= "            through this caller chain. Always 100% on the\n"
  sb ++= "            leaf row itself; sums of direct callers ~ 100%\n"
  sb ++= "            modulo threshold-pruned siblings.\n\n"
  sb ++= f"  ${"tot%"}%6s ${"leaf%"}%6s  call tree\n"
  val top = leafCounts.toSeq.sortBy { case (_, c) => -c }.take(topN)
  for (leafMethod, leafCount) <- top do
    val root = buildCallerTree(samples, leafMethod)
    val leafPct = 100.0 * leafCount / kept
    val floor = leafCount * leafPctThreshold / 100.0
    sb ++= "\n  ---\n"
    sb ++= f"  $leafPct%6.2f ${100.0}%6.2f  * $leafMethod\n"
    def walk(node: TreeNode, depth: Int): Unit =
      if depth > maxDepth then return
      val kids = node.children.values.toArray.sortBy(k => -k.total)
      var i = 0
      while i < kids.length do
        val kid = kids(i)
        if kid.total >= floor then
          val tot = 100.0 * kid.total / kept
          val leaf = 100.0 * kid.total / leafCount
          sb ++= f"  $tot%6.2f $leaf%6.2f  ${"  " * depth}^ ${kid.name}\n"
          walk(kid, depth + 1)
        i += 1
    walk(root, 0)

private def appendAllocForest(sb: StringBuilder, allocs: Vector[JfrAllocSample],
                              bytesByClass: Map[String, Long], totalBytes: Long,
                              nEvents: Int, topN: Int, classPctThreshold: Double,
                              maxDepth: Int): Unit =
  if totalBytes <= 0 || topN <= 0 then return
  sb ++= f"\n=== Allocation forest (top $topN classes, caller >= $classPctThreshold%.1f%% of class bytes) ===\n"
  sb ++= f"  TLAB-allocation events: $nEvents\n"
  sb ++= f"  Approx total bytes:     ${fmtBytes(totalBytes)}\n"
  sb ++= "  Each `---` block has a class (marked `*`) followed by its\n"
  sb ++= "  caller chain (marked `^`, walking OUT toward main).\n"
  sb ++= "  Columns:\n"
  sb ++= "    tot%   = % of ALL allocation bytes whose stack contains\n"
  sb ++= "             this caller-chain ending at the class.\n"
  sb ++= "    class% = % of the class's own bytes that came in through\n"
  sb ++= "             this caller chain. 100% on the class row itself.\n\n"
  sb ++= f"  ${"tot%"}%6s ${"class%"}%6s  ${"bytes"}%10s  call tree\n"
  val top = bytesByClass.toSeq.sortBy { case (_, b) => -b }.take(topN)
  for (cls, clsBytes) <- top if clsBytes > 0 do
    val root = buildAllocCallerTree(allocs, cls)
    val clsPct = 100.0 * clsBytes / totalBytes
    val floor = clsBytes * classPctThreshold / 100.0
    sb ++= "\n  ---\n"
    sb ++= f"  $clsPct%6.2f ${100.0}%6.2f  ${fmtBytes(clsBytes)}  * $cls\n"
    def walk(node: TreeNode, depth: Int): Unit =
      if depth > maxDepth then return
      val kids = node.children.values.toArray.sortBy(k => -k.total)
      var i = 0
      while i < kids.length do
        val kid = kids(i)
        if kid.total >= floor then
          val tot = 100.0 * kid.total / totalBytes
          val cp  = 100.0 * kid.total / clsBytes
          sb ++= f"  $tot%6.2f $cp%6.2f  ${fmtBytes(kid.total)}  ${"  " * depth}^ ${kid.name}\n"
          walk(kid, depth + 1)
        i += 1
    walk(root, 0)

/** Build the three call-tree views from one JFR's worth of frames and write
 *  them to `out`. Thresholds match the Python original; tweak via params. */
def writeTreeReport(analysis: JfrAnalysis, out: os.Path,
                    topDownThresholdPct: Double = 1.0, topDownMaxDepth: Int = 30,
                    reverseTop: Int = 20, reverseThresholdPct: Double = 5.0,
                    reverseMaxDepth: Int = 20,
                    allocTop: Int = 10, allocThresholdPct: Double = 5.0,
                    allocMaxDepth: Int = 20): Unit =
  val st = analysis.stats
  val sb = StringBuilder()
  sb ++= s"=== Tree report (single profile run) ===\n"
  sb ++= s"  samples:      ${st.kept}\n"
  sb ++= s"  alloc events: ${st.nAllocEvents}\n"
  sb ++= s"  alloc bytes:  ${fmtBytes(st.totalBytes).trim}\n"
  appendTopDown(sb, buildTopDown(analysis.samples), st.kept, topDownThresholdPct, topDownMaxDepth)
  appendCallerForest(sb, analysis.samples, st.leafCounts, st.kept,
                     reverseTop, reverseThresholdPct, reverseMaxDepth)
  appendAllocForest(sb, analysis.allocs, st.bytesByClass, st.totalBytes,
                    st.nAllocEvents, allocTop, allocThresholdPct, allocMaxDepth)
  os.write.over(out, sb.toString, createFolders = true)

def jfrJvmOpts(jfrOut: os.Path): Seq[String] = Seq(
  "-Xms2g", "-Xmx4g",
  "-XX:FlightRecorderOptions=stackdepth=256",
  s"-XX:StartFlightRecording=filename=$jfrOut,settings=profile," +
    "jdk.ExecutionSample#period=1ms," +
    "jdk.ObjectAllocationInNewTLAB#enabled=true," +
    "jdk.ObjectAllocationInNewTLAB#stackTrace=true," +
    "jdk.ObjectAllocationOutsideTLAB#enabled=true," +
    "jdk.ObjectAllocationOutsideTLAB#stackTrace=true," +
    "dumponexit=true",
)

def profileJfrPath(jfrOut: os.Path, repeat: Int, idx: Int): os.Path =
  if repeat > 1 then
    val stem = jfrOut.last.stripSuffix(".jfr")
    jfrOut / os.up / s"$stem-run$idx.jfr"
  else jfrOut

def removeProfileJfrFiles(jfrOut: os.Path, repeat: Int): Int =
  var removed = 0
  for jfr <- (1 to repeat).map(i => profileJfrPath(jfrOut, repeat, i)).distinct do
    if os.isFile(jfr) then
      os.remove(jfr)
      removed += 1
  removed

def recordAndAnalyze(repeat: Int, runs: Int, jfrOut: os.Path, compilerCp: String,
                     threadName: String = "main")(using env: Env): Unit =
  if repeat < 1 then sys.error(s"repeat must be >= 1, got $repeat")
  val stem = jfrOut.last.stripSuffix(".jfr")
  os.makeDir.all(jfrOut / os.up)
  val all = mutable.ListBuffer.empty[RunStats]
  for i <- 1 to repeat do
    val jfr = profileJfrPath(jfrOut, repeat, i)
    if repeat > 1 then println(s"\n=== profile repeat $i/$repeat: ${jfr.last} ===")
    os.remove(jfr, checkExists = false)
    println(s"[profile] runs=$runs  JFR=on -> $jfr")
    val rc = runProfileRunner(runs, jfrJvmOpts(jfr), Nil, compilerCp)
    if rc != 0 then sys.error(s"profile run failed (rc=$rc)")
    if !os.exists(jfr) then sys.error(s"expected JFR at $jfr, not found after run")
    val t0 = System.nanoTime()
    // First run also retains per-event stack frames so we can build call
    // trees. The remaining runs only need the flat counts that
    // writeSummary aggregates, so we skip the per-sample buffers there.
    val (st, treesWritten) =
      if i == 1 then
        val full = analyzeJfrFull(jfr, threadName)
        val treePath = jfrOut / os.up / s"$stem-trees.txt"
        writeTreeReport(full, treePath)
        (full.stats, Some(treePath))
      else (analyzeJfr(jfr, threadName), None)
    val ms = (System.nanoTime() - t0) / 1_000_000
    val tail = treesWritten.fold("")(p => s"; trees -> ${p.last}")
    println(s"[profile] analyzed in ${ms}ms: ${st.kept} samples, ${st.nAllocEvents} alloc events, ${fmtBytes(st.totalBytes).trim} alloc$tail")
    all += st
  val summaryPath = jfrOut / os.up / s"$stem-summary.txt"
  writeSummary(all.toSeq, summaryPath)
  println(s"[profile] summary saved to $summaryPath")
  val removed = removeProfileJfrFiles(jfrOut, repeat)
  if removed > 0 then println(s"[profile] removed $removed JFR file(s)")

// Mill corpus setup.

val MillVersion = "1.1.6"
val MillScalaModules = Seq(
  "libs-javalib", "libs-util", "libs-rpc", "libs-javalib-api", "libs-javalib-testrunner",
  "core-api", "core-api-daemon", "core-api-java11",
  "libs-daemon-server", "libs-daemon-client", "libs-util-java11",
)
val MillJavaModules = Seq("core-constants", "libs-javalib-testrunner-entrypoint")
val ScalaPluginBinaryVersion = "3.8.2"
val MillModuledefsVersion = "0.13.1"
val Plugins = Seq(
  s"com.lihaoyi:scalac-mill-moduledefs-plugin_$ScalaPluginBinaryVersion:$MillModuledefsVersion",
  "com.lihaoyi:unroll-plugin_3:0.2.0",
)
val ExtraCompileDeps = Seq(s"org.scala-lang:scala3-compiler_3:$ScalaPluginBinaryVersion")
val ScalacOptionsTail = Seq("-deprecation", "-feature", "-Xkind-projector:underscores")
val SetupMarkers = Seq("compile-classpath.txt", "scalac-options.txt", "source-files.txt")

def millCoords(version: String): Seq[String] =
  MillScalaModules.map(m => s"com.lihaoyi:mill-${m}_3:$version") ++
    MillJavaModules.map(m => s"com.lihaoyi:mill-$m:$version")

def csFetch(coords: Seq[String], sources: Boolean = false): Seq[os.Path] =
  val srcFlag = if sources then Seq("--sources") else Nil
  os.call(("cs", "fetch", srcFlag, "-p", coords)).out.text().trim
    .split(java.io.File.pathSeparator).iterator
    .filter(_.nonEmpty).map(os.Path(_)).toSeq

def isMillJar(p: os.Path): Boolean = p.last.startsWith("mill-") && p.last.endsWith(".jar")

def isSetupComplete(using env: Env): Boolean =
  SetupMarkers.forall(m => os.isFile(env.inputsDir / m)) &&
    os.exists(env.sourcesDir) && os.list(env.sourcesDir).nonEmpty

def runSetup(millVersion: String)(using env: Env): Unit =
  os.makeDir.all(env.inputsDir)
  val coords = millCoords(millVersion)
  println(s"[setup] Mill version: $millVersion")

  val srcJars = csFetch(coords, sources = true)
    .filter(j => j.last.endsWith("-sources.jar") && j.last.startsWith("mill-"))
  println(s"[setup] fetched ${srcJars.size} mill source jars; extracting -> ${env.sourcesDir}")
  os.remove.all(env.sourcesDir)
  os.makeDir.all(env.sourcesDir)
  var nFiles = 0
  for j <- srcJars do
    Using.resource(java.util.zip.ZipFile(j.toIO)) { zf =>
      val entries = zf.entries()
      while entries.hasMoreElements do
        val e = entries.nextElement()
        val nm = e.getName
        if !e.isDirectory && (nm.endsWith(".scala") || nm.endsWith(".java")) then
          Using.resource(zf.getInputStream(e)) { is =>
            os.write.over(env.sourcesDir / os.SubPath(nm), is, createFolders = true)
          }
          nFiles += 1
    }
  println(s"[setup]   $nFiles source files extracted")

  val srcList = os.walk(env.sourcesDir).filter(os.isFile)
    .filter(p => p.last.endsWith(".scala") || p.last.endsWith(".java"))
    .map(_.relativeTo(env.sourcesDir).toString).sorted
  os.write.over(env.inputsDir / "source-files.txt", srcList.mkString("\n") + "\n")
  println(s"[setup]   source-files.txt: ${srcList.size} entries")

  val cpJars = csFetch(coords ++ ExtraCompileDeps).filterNot(isMillJar)
  os.write.over(env.inputsDir / "compile-classpath.txt", cpJars.mkString("\n") + "\n")
  println(s"[setup]   compile-classpath.txt: ${cpJars.size} entries")

  val pluginJars = csFetch(Plugins).filter { j =>
    j.last.endsWith(".jar") && Plugins.exists(c => j.last.contains(c.split(":")(1).split("_")(0)))
  }.distinctBy(_.last)
  val opts = pluginJars.map(j => s"-Xplugin:$j") ++ ScalacOptionsTail
  os.write.over(env.inputsDir / "scalac-options.txt", opts.mkString("\n") + "\n")
  println(s"[setup]   scalac-options.txt: ${opts.size} entries (${pluginJars.size} plugins)")
  println("[setup] Done.")
