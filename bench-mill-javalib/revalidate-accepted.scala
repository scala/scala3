//| moduleDeps: [Common.scala]
//| scalaVersion: 3.8.2

// Re-profile the baseline and every commit on `accepted`.
//
// This intentionally does not parse existing commit messages and does not
// rewrite branches. It only creates an iter-N/run-M series and a manifest so
// the JFR rows can be checked with `delta.scala` and the commit messages can
// be rewritten manually.

import java.nio.file.{FileAlreadyExistsException, Files}
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

case class Commit(idx: Int, sha: String, subject: String):
  def sha7: String = sha.take(7)

class FatalRunException(message: String) extends RuntimeException(message)

def main(
    acceptedBranch: String = "accepted",
    base: String = "bench-mill-javalib",
    worktree1: String = (Scala3Dir / os.up / "scala3-revalidate-accepted").toString,
    worktree2: String = (Scala3Dir / os.up / "scala3-revalidate-accepted-2").toString,
    worktree3: String = (Scala3Dir / os.up / "scala3-revalidate-accepted-3").toString,
    worktree4: String = (Scala3Dir / os.up / "scala3-revalidate-accepted-4").toString,
    workers: Int = 4,
    repeat: Int = 5,
    runs: Int = 10,
    thread: String = "main",
    out: String = "",
    limit: Int = 0,
    forceProfile: Boolean = false,
    profileAttempts: Int = 3,
    minFreeGiB: Int = 30,
    compileOnly: Boolean = false,
): Unit =
  val commits0 = gitLines("log", "--reverse", "--format=%H%x09%s", s"$base..$acceptedBranch").zipWithIndex.map {
    case (line, i) =>
      val Array(sha, subject) = line.split("\t", 2)
      Commit(i + 1, sha, subject)
  }
  val commits = if limit > 0 then commits0.take(limit) else commits0
  if commits.isEmpty then sys.error(s"no commits found in $base..$acceptedBranch")

  if compileOnly then
    println(s"[revalidate-accepted] compileOnly: found ${commits0.size} commits in $base..$acceptedBranch")
    return

  val outBase =
    if out.nonEmpty then os.Path(out, os.pwd)
    else claimIterDir(BenchDir)
  os.makeDir.all(outBase)
  requireFreeSpace(outBase, minFreeGiB)

  val worktrees = Vector(worktree1, worktree2, worktree3, worktree4).take(workers.max(1).min(4)).map(os.Path(_, os.pwd))
  worktrees.foreach(ensureWorktree(_, base))

  val baseSha = git("rev-parse", base)
  worktrees.foreach { wt =>
    println(s"[revalidate-accepted] reset ${wt.last} to base ${baseSha.take(7)}")
    checkout(wt, baseSha)
    compileBase(wt)
  }
  val all = Commit(0, baseSha, s"BASE $base") +: commits

  println(s"[revalidate-accepted] commits=${commits.size}/${commits0.size}, output=$outBase")
  println(s"[revalidate-accepted] worktrees=${worktrees.mkString(", ")}")

  val manifest = StringBuilder()
  manifest ++= "idx\tsha\trun\tparentRun\tsubject\n"
  for (commit, i) <- all.zipWithIndex do
    val parentRun = if i == 0 then "" else runLabel(outBase, i - 1)
    manifest ++= s"${commit.idx}\t${commit.sha}\t${runLabel(outBase, i)}\t$parentRun\t${commit.subject}\n"
  os.write.over(outBase / "manifest.tsv", manifest.toString, createFolders = true)
  os.write.over(outBase / "progress.tsv", "idx\tsha\tstatus\trun\telapsedSec\tdetail\n", createFolders = true)

  val baseRun = outBase / "run-0"
  if forceProfile || !os.exists(baseRun / "profile-summary.txt") then
    requireFreeSpace(outBase, minFreeGiB)
    profileCommit(worktrees.head, baseSha, baseRun, repeat, runs, thread, profileAttempts, skipBuild = true)
  else
    println(s"[profile] reuse ${baseSha.take(7)} -> ${runLabel(outBase, 0)}")

  val queue = ConcurrentLinkedQueue[(Commit, Int)]()
  commits.zipWithIndex.foreach { case (c, i) => queue.add((c, i + 1)) }
  val started = AtomicInteger(0)
  val failed = AtomicInteger(0)
  val abort = AtomicBoolean(false)
  val total = commits.size
  val start = System.nanoTime()

  val threads = worktrees.zipWithIndex.map { case (wt, workerIdx) =>
    Thread(() => {
      var next = Option(queue.poll())
      while next.nonEmpty && !abort.get() do
        val (commit, runIdx) = next.get
        val n = started.incrementAndGet()
        val runDir = outBase / s"run-$runIdx"
        val summary = runDir / "profile-summary.txt"
        val prefix = s"[worker-${workerIdx + 1}] ${commit.sha7} $n/$total"
        try
          if forceProfile || !os.exists(summary) then
            println(s"$prefix -> ${runLabel(outBase, runIdx)}")
            requireFreeSpace(outBase, minFreeGiB)
            profileCommit(wt, commit.sha, runDir, repeat, runs, thread, profileAttempts)
            appendProgress(outBase, s"${commit.idx}\t${commit.sha}\tok\t${runLabel(outBase, runIdx)}\t${elapsedSec(start)}\t")
          else
            println(s"$prefix reuse ${runLabel(outBase, runIdx)}")
            appendProgress(outBase, s"${commit.idx}\t${commit.sha}\treuse\t${runLabel(outBase, runIdx)}\t${elapsedSec(start)}\t")
        catch case e: Throwable =>
          failed.incrementAndGet()
          val detail = Option(e.getMessage).getOrElse(e.getClass.getName).replace('\t', ' ').replace('\n', ' ')
          println(s"$prefix failed: $detail")
          appendProgress(outBase, s"${commit.idx}\t${commit.sha}\tfail\t${runLabel(outBase, runIdx)}\t${elapsedSec(start)}\t$detail")
          if isFatalInfrastructureFailure(e) then abort.set(true)
        next = if abort.get() then None else Option(queue.poll())
    }, s"revalidate-accepted-${workerIdx + 1}")
  }

  threads.foreach(_.start())
  threads.foreach(_.join())
  if abort.get() then sys.error(s"aborted after fatal infrastructure failure; see ${outBase / "progress.tsv"}")
  if failed.get() != 0 then sys.error(s"${failed.get()} commit(s) failed; see ${outBase / "progress.tsv"}")

  println(s"[revalidate-accepted] wrote ${outBase / "manifest.tsv"}")
  println("[revalidate-accepted] use delta.scala manually, e.g.:")
  println(s"  ./mill bench-mill-javalib/delta.scala --before ${runLabel(outBase, 0)} --after ${runLabel(outBase, 1)} METHOD")

def profileCommit(
    wt: os.Path,
    sha: String,
    runDir: os.Path,
    repeat: Int,
    runs: Int,
    thread: String,
    profileAttempts: Int,
    skipBuild: Boolean = false,
): Unit =
  println(s"[profile] ${sha.take(7)} -> ${runDir.relativeTo(BenchDir)}")
  checkout(wt, sha)
  val attempts = profileAttempts.max(1)
  var attempt = 1
  var lastFailure = ""
  while attempt <= attempts do
    cleanRunDirForAttempt(runDir)
    val log = runDir / "profile.log"
    os.write.over(log, s"[profile] ${sha.take(7)} in $wt, attempt $attempt/$attempts\n", createFolders = true)
    val skipBuildThisAttempt = skipBuild || attempt > 1
    val result = os.call(
      cmd = ((wt / "mill").toString, "bench-mill-javalib/profile.scala",
             "--repeat", repeat.toString,
             "--runs", runs.toString,
             "--jfr-out", (runDir / "profile.jfr").toString,
             "--thread", thread,
             "--skip-build", skipBuildThisAttempt.toString),
      cwd = wt,
      stdout = os.ProcessOutput.Readlines(line => os.write.append(log, line + "\n")),
      mergeErrIntoOut = true,
      check = false,
    )
    val summary = runDir / "profile-summary.txt"
    val invalidJfr = hasJfrSamplerFailure(log)
    val missingSummary = !os.exists(summary)
    if result.exitCode == 0 && !missingSummary && !invalidJfr then
      cleanupJfrFiles(runDir)
      return

    lastFailure =
      if invalidJfr then s"JFR sampler failed; see $log"
      else if result.exitCode != 0 then s"profile.scala failed (rc=${result.exitCode}); see $log"
      else s"profile.scala did not write $summary"

    if invalidJfr && attempt < attempts then
      val invalidLog = runDir / s"profile-invalid-attempt-$attempt.log"
      os.move.over(log, invalidLog)
      cleanupJfrFiles(runDir)
      println(s"[profile] ${sha.take(7)} retrying after JFR sampler failure; saved ${invalidLog.last}")
    else sys.error(lastFailure)

    attempt += 1
  sys.error(lastFailure)

def cleanRunDirForAttempt(runDir: os.Path): Unit =
  os.makeDir.all(runDir)
  for p <- os.list(runDir) do
    if !p.last.startsWith("profile-invalid-attempt-") then os.remove.all(p)

def cleanupJfrFiles(runDir: os.Path): Unit =
  if os.exists(runDir) then
    for p <- os.list(runDir) do
      if os.isFile(p) && p.last.endsWith(".jfr") then os.remove(p)

def hasJfrSamplerFailure(log: os.Path): Boolean =
  os.exists(log) && os.read.lines(log).exists { line =>
    line.contains("Thread method sampler crashed") || line.contains("][error][jfr")
  }

def requireFreeSpace(path: os.Path, minFreeGiB: Int): Unit =
  if minFreeGiB > 0 then
    os.makeDir.all(path)
    val usable = Files.getFileStore(path.toNIO).getUsableSpace
    val min = minFreeGiB.toLong * 1024L * 1024L * 1024L
    if usable < min then
      throw FatalRunException(s"only ${formatGiB(usable)} free at $path; need at least ${minFreeGiB}GiB")

def formatGiB(bytes: Long): String =
  f"${bytes.toDouble / 1024 / 1024 / 1024}%.1fGiB"

def isFatalInfrastructureFailure(e: Throwable): Boolean =
  val msg = Iterator.iterate(e)(_.getCause).takeWhile(_ != null)
    .map(t => Option(t.getMessage).getOrElse(t.getClass.getName)).mkString("\n")
  e.isInstanceOf[FatalRunException] ||
    msg.contains("No space left on device") ||
    msg.contains("not enough free space")

private val progressLock = Object()

def appendProgress(outBase: os.Path, line: String): Unit =
  progressLock.synchronized {
    os.write.append(outBase / "progress.tsv", line + "\n", createFolders = true)
  }

def elapsedSec(start: Long): Long =
  (System.nanoTime() - start) / 1_000_000_000L

def ensureWorktree(wt: os.Path, ref: String): Unit =
  if os.exists(wt / ".git") then
    val status = os.call(("git", "status", "--porcelain"), cwd = wt).out.text().trim
    if status.nonEmpty then sys.error(s"worktree has local changes: $wt")
  else
    os.makeDir.all(wt / os.up)
    os.call(("git", "worktree", "add", "--detach", wt.toString, ref), cwd = Scala3Dir, stdout = os.Inherit, stderr = os.Inherit)

def checkout(wt: os.Path, sha: String): Unit =
  os.call(("git", "checkout", "-q", "--detach", sha), cwd = wt)

def compileBase(wt: os.Path): Unit =
  println(s"[revalidate-accepted] compile base in ${wt.last}")
  os.call(
    ("sbt", "scala3-compiler-nonbootstrapped/compile"),
    cwd = wt,
    stdout = os.Inherit,
    stderr = os.Inherit,
  )

def claimIterDir(benchDir: os.Path): os.Path =
  Files.createDirectories(benchDir.toNIO)
  var n =
    if !os.exists(benchDir) then 0
    else
      os.list(benchDir).iterator.flatMap { p =>
        Option.when(os.isDir(p) && p.last.startsWith("iter-"))(p.last.stripPrefix("iter-").toIntOption).flatten
      }.maxOption.getOrElse(-1) + 1
  while true do
    val dir = benchDir / s"iter-$n"
    try
      Files.createDirectory(dir.toNIO)
      return dir
    catch case _: FileAlreadyExistsException =>
      n += 1
  sys.error("unreachable")

def runLabel(outBase: os.Path, idx: Int): String =
  val runDir = outBase / s"run-$idx"
  scala.util.Try(runDir.relativeTo(BenchDir).toString).getOrElse(runDir.toString)

def git(args: String*): String = gitIn(Scala3Dir, args*)
def gitLines(args: String*): Vector[String] = git(args*).linesIterator.filter(_.nonEmpty).toVector

def gitIn(cwd: os.Path, args: String*): String =
  os.call(("git", args), cwd = cwd).out.text().trim
