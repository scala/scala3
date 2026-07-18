//> using scala 3.7
//> using toolkit default

import scala.util.matching.Regex

import scala.util.CommandLineParser.FromString

given FromString[os.Path] with
  def fromString(s: String) = os.Path(s, os.pwd)

/** Sync standard library changes from scala/scala to scala/scala3.
  *
  * Usage: scala run syncStdLib.scala -- <scala2-repo-path> <from-tag> <to-tag>
  *
  * Example: scala syncStdLib.scala -- ../scala2 v2.13.17 v2.13.18
  */
@main def syncStdLib(
    scala2RepoPath: os.Path,
    fromTag: String,
    toTag: String
): Unit =
  val scala2Path = os.Path(scala2RepoPath, os.pwd)
  val scala3Path = os.pwd

  val workDir = scala3Path / "stdlib-sync-work"
  val patchDir = workDir / "patches"

  println(s"Scala 2 repo: $scala2Path")
  println(s"Scala 3 repo: $scala3Path")
  println(s"Syncing: $fromTag..$toTag")
  println()

  // Clean up and create directories
  os.remove.all(workDir)
  os.makeDir.all(patchDir)

  // -------------------------
  // 1) Fetch latest changes in scala2 repo
  // -------------------------
  println(s"Fetching tags $fromTag and $toTag from scala2...")
  os.proc("git", "fetch", "--tags", "origin", fromTag, toTag)
    .call(cwd = scala2Path, stdout = os.Inherit, stderr = os.Inherit)

  // Show what will be synced
  println(s"\nCommits to sync ($fromTag..$toTag):")
  val logResult = os
    .proc("git", "log", "--oneline", s"$fromTag..$toTag", "--", "src/library")
    .call(cwd = scala2Path)
  logResult.out.lines().foreach(println)
  println()

  // -------------------------
  // 2) Create patch series for src/library only, paths relative to src/library/
  // -------------------------
  println("Creating patches...")
  os.proc(
    "git",
    "format-patch",
    "--output-directory",
    patchDir.toString,
    "--relative=src/library",
    "--no-signature",
    s"$fromTag..$toTag",
    "--",
    "src/library"
  ).call(cwd = scala2Path, stdout = os.Inherit, stderr = os.Inherit)

  val patchFiles = os.list(patchDir).filter(_.ext == "patch").sorted
  println(s"Created ${patchFiles.size} patches")

  // -------------------------
  // 3) Rewrite patches:
  //    - qualify #NNNN -> scala/scala#NNNN
  //    - add footer: "Upstream: scala/scala@<sha>"
  // -------------------------
  println("\nRewriting patches...")
  rewritePatches(patchDir)

  // -------------------------
  // 4) Apply onto scala3
  // -------------------------
  val branchName = s"stdlib-sync/${toTag.stripPrefix("v")}"
  println(s"\nCreating branch $branchName...")

  os.proc("git", "checkout", "-b", branchName, "origin/main")
    .call(cwd = scala3Path, stdout = os.Inherit, stderr = os.Inherit)

  println(s"Applying ${patchFiles.size} patches...")

  val patches = os.list(patchDir).filter(_.ext == "patch").sorted
  val amResult = os
    .proc("git", "am", "--3way", "--directory=library/src", patches)
    .call(
      cwd = scala3Path,
      check = false,
      stdout = os.Inherit,
      stderr = os.Inherit
    )

  if amResult.exitCode != 0 then
    println()
    println("Conflicts occurred.")
    println("Resolve, then run: git am --continue")
    println("Abort with:        git am --abort")
    sys.exit(1)

  println("\nPatches applied successfully!")
  os.remove.all(workDir)
  
  println(s"\nTo push the branch, run:")
  println(s"  git push -u origin $branchName")
end syncStdLib

/** Rewrite patches to:
  *   - Qualify GitHub issue refs: #NNNN -> scala/scala#NNNN
  *   - Add footer: "Upstream: scala/scala@<sha>"
  */
def rewritePatches(patchDir: os.Path): Unit =
  val patchFiles = os.list(patchDir).filter(_.ext == "patch").sorted
  val issueRefPattern = """(?<!\w)#(\d+)""".r
  val fromLinePattern = """^From ([0-9a-f]{40}) """.r

  var rewrittenCount = 0

  for patch <- patchFiles do
    val raw = os.read(patch)

    // Extract upstream commit SHA from the "From <sha> ..." line
    val upstream = fromLinePattern.findFirstMatchIn(raw).map(_.group(1))

    // Qualify github refs in the patch (commit message is inside)
    var rewritten = issueRefPattern.replaceAllIn(raw, "scala/scala#$1")

    // Insert "Upstream: ..." footer into the commit message.
    // In format-patch output, commit message ends just before "\n---\n"
    upstream.foreach { sha =>
      val footer = s"Upstream: scala/scala@$sha"
      if !rewritten.contains(footer) then
        rewritten.split("\n---\n", 2) match
          case Array(msg, rest) =>
            var newMsg = msg
            if !newMsg.endsWith("\n") then newMsg += "\n"
            if !newMsg.endsWith("\n\n") then newMsg += "\n"
            newMsg += s"$footer\n"
            rewritten = newMsg + "\n---\n" + rest
          case _ => // No split point found, leave as is
    }

    if rewritten != raw then
      os.write.over(patch, rewritten)
      rewrittenCount += 1

  println(s"Rewrote $rewrittenCount patches")
end rewritePatches
