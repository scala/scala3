//| moduleDeps: [Common.scala]
//| scalaVersion: 3.8.2

// Profile every commit on two branches (default: `accepted`, `rejected`) in
// two worktrees in parallel, with work-stealing — once a worker drains its
// own branch's queue it steals from the other branch.
//
// Calls Common.scala's profiling functions directly, threading a per-worker
// `Env` so each worker operates on its own worktree without spawning a
// fresh Mill subprocess per commit.
//
// Each baseline (the `base` branch HEAD) is profiled first in worktree1
// before workers start — its profile is the parent for the first commit on
// each branch's delta computation.

import java.util.concurrent.ConcurrentLinkedDeque
import java.util.concurrent.atomic.AtomicInteger

case class Work(branch: String, idx: Int, sha: String):
  def sha7: String = sha.take(7)
  override def toString = f"$branch%-8s ${idx}%02d $sha7"

def main(
    acceptedBranch: String = "accepted",
    rejectedBranch: String = "rejected",
    base: String = "bench-mill-javalib",
    worktree1: String = (Scala3Dir / os.up / "scala3-1").toString,
    worktree2: String = (Scala3Dir / os.up / "scala3-2").toString,
    out: String = (BenchDir / "revalidate-mill").toString,
): Unit =
  val outBase = os.Path(out, os.pwd)
  os.makeDir.all(outBase)
  val wt1 = os.Path(worktree1, os.pwd)
  val wt2 = os.Path(worktree2, os.pwd)

  val acceptedShas = gitLog(Scala3Dir, s"$base..$acceptedBranch")
  val rejectedShas = gitLog(Scala3Dir, s"$base..$rejectedBranch")
  println(s"[revalidate] accepted=${acceptedShas.size}, rejected=${rejectedShas.size}; worktrees: $wt1, $wt2")

  val acceptedQ = dequeOf(acceptedShas.zipWithIndex.map((s, i) => Work("accepted", i + 1, s)))
  val rejectedQ = dequeOf(rejectedShas.zipWithIndex.map((s, i) => Work("rejected", i + 1, s)))
  val total = acceptedQ.size + rejectedQ.size
  val done = AtomicInteger(0)
  val tStart = System.nanoTime()

  for wt <- Seq(wt1, wt2) do
    println(s"[revalidate] fetching in $wt …")
    os.call(("git", "fetch", "scala3-main", base, acceptedBranch, rejectedBranch),
            cwd = wt, check = false, stdout = os.Inherit, stderr = os.Inherit)

  // Profile the baseline (parent of accepted[0] and rejected[0]) in worktree1
  // before launching the per-branch workers. The output lands at
  // `<out>/base/profile-summary.txt`. This is the parent profile any
  // downstream delta-analysis needs for the first commit on each branch.
  val baseSha = os.call(("git", "rev-parse", base), cwd = Scala3Dir).out.text().trim
  profileBaseline(wt1, base, baseSha, outBase)

  def worker(name: String, wt: os.Path,
             primary: ConcurrentLinkedDeque[Work], secondary: ConcurrentLinkedDeque[Work]): Unit =
    given Env = Env(wt)
    println(s"[$name] starting in $wt")
    if !isSetupComplete then
      println(s"[$name] running corpus setup")
      runSetup(MillVersion)

    def pollNext(): Option[(Work, Boolean)] =
      Option(primary.pollFirst()).map((_, false))
        .orElse(Option(secondary.pollFirst()).map((_, true)))

    var next = pollNext()
    while next.isDefined do
      val (work, stole) = next.get
      val n = done.incrementAndGet()
      val secs = (System.nanoTime() - tStart) / 1_000_000_000L
      println(s"[$name] $work — $n/$total (${secs}s elapsed${if stole then ", stealing" else ""})")
      runOne(wt, work, outBase)
      next = pollNext()
    println(s"[$name] queue drained")

  val t1 = Thread(() => worker("worker1", wt1, acceptedQ, rejectedQ))
  val t2 = Thread(() => worker("worker2", wt2, rejectedQ, acceptedQ))
  t1.start(); t2.start()
  t1.join(); t2.join()

  val secs = (System.nanoTime() - tStart) / 1_000_000_000L
  println(s"[revalidate] DONE: $done/$total commits, elapsed=${secs}s")

def profileBaseline(wt: os.Path, baseRef: String, baseSha: String, outBase: os.Path): Unit =
  val outDir = outBase / "base"
  os.makeDir.all(outDir)
  val tStart = System.nanoTime()
  println(s"[base] profiling $baseRef ($baseSha) in $wt")
  os.call(("git", "checkout", "-q", "--detach", baseSha), cwd = wt, check = true)
  given env: Env = Env(wt)
  if !isSetupComplete then
    println(s"[base] running corpus setup")
    runSetup(MillVersion)
  val cp = compilerClasspath()
  recordAndAnalyze(repeat = 5, runs = 10, jfrOut = outDir / "profile.jfr", compilerCp = cp)
  appendLog(outBase, "base", "progress", f"00 ${baseSha.take(7)} ok      ${elapsedSec(tStart)}%4d")

def gitLog(repo: os.Path, range: String): Seq[String] =
  os.call(("git", "log", "--reverse", "--format=%H", range), cwd = repo).out.lines()
    .filter(_.nonEmpty)

def dequeOf[T](xs: Seq[T]): ConcurrentLinkedDeque[T] =
  val d = ConcurrentLinkedDeque[T]()
  xs.foreach(d.offer)
  d

private val logLock = Object()

def runOne(worktree: os.Path, w: Work, outBase: os.Path)(using env: Env): Unit =
  val outDir = outBase / w.branch / f"${w.idx}%02d-${w.sha7}"
  val summary = outDir / "profile-summary.txt"
  val tStart = System.nanoTime()

  os.remove.all(outDir)
  os.makeDir.all(outDir)

  val coRc = os.call(("git", "checkout", "-q", "--detach", w.sha), cwd = worktree, check = false).exitCode
  if coRc != 0 then
    fail(outBase, w, "checkout failed", tStart)
    return

  try
    val cp = compilerClasspath()
    recordAndAnalyze(repeat = 5, runs = 10, jfrOut = outDir / "profile.jfr", compilerCp = cp)
  catch case e: Throwable =>
    fail(outBase, w, s"profile failed: ${e.getMessage}", tStart)
    return

  if !os.exists(summary) || os.read(summary).contains("samples per run:      0, 0, 0") then
    fail(outBase, w, "no/empty summary produced", tStart)
  else
    appendLog(outBase, w.branch, "progress",
              f"${w.idx}%02d ${w.sha7} ok      ${elapsedSec(tStart)}%4d")

def fail(outBase: os.Path, w: Work, reason: String, tStart: Long): Unit =
  appendLog(outBase, w.branch, "failures", f"${w.idx}%02d ${w.sha7} $reason")
  appendLog(outBase, w.branch, "progress",
            f"${w.idx}%02d ${w.sha7} fail    ${elapsedSec(tStart)}%4d  $reason")

def appendLog(outBase: os.Path, branch: String, kind: String, line: String): Unit =
  logLock.synchronized {
    os.write.append(outBase / s"$branch-$kind.log", line + "\n", createFolders = true)
  }

def elapsedSec(tStart: Long): Long = (System.nanoTime() - tStart) / 1_000_000_000L
