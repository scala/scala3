//> using scala 3.8
//> using toolkit default

val docsDir         = os.pwd / "docs"

val sidebarPath        = docsDir / "sidebar.yml"
val nightlySidebarPath = docsDir / "sidebar.nightly.template.yml"

val referenceDir       = docsDir / "_docs" / "reference"
val contributingDir    = docsDir / "_docs" / "contributing"
val internalsDir       = docsDir / "_docs" / "internals"

final val SidebarEntry = raw"""^\s*(?:-\s*)?(page|index):\s*(\S+)""".r

val referenceIgnored = Set[String]()
val internalsIgnored = Set[String](
  "internals/cc/use-design.md",
  "internals/cc/alternatives-to-sealed.md",
  "internals/cc/handling-invariant-vars.md",
  "internals/exclusive-capabilities.md",
)

@main def checkSidebarDocs(): Unit = {
  val issues = Vector.newBuilder[String]

  val referenceRoot = referenceDir / os.up
  val sidebarEntries = readSidebarEntries(sidebarPath)
  issues ++= missingReferencedPaths(sidebarPath, sidebarEntries, referenceRoot)
  issues ++= missingFromSidebar(
    label = "reference",
    docsDir = referenceDir,
    docsRoot = referenceRoot,
    sidebarEntries = sidebarEntries,
    ignored = referenceIgnored
  )

  val nightlyRoot = contributingDir / os.up
  val nightlyEntries = readSidebarEntries(nightlySidebarPath)
  issues ++= missingReferencedPaths(nightlySidebarPath, nightlyEntries, nightlyRoot)
  issues ++= missingFromSidebar(
    label = "contributing",
    docsDir = contributingDir,
    docsRoot = nightlyRoot,
    sidebarEntries = nightlyEntries,
    ignored = Set.empty
  )
  issues ++= missingFromSidebar(
    label = "internals",
    docsDir = internalsDir,
    docsRoot = nightlyRoot,
    sidebarEntries = nightlyEntries,
    ignored = internalsIgnored
  )

  val problems = issues.result()
  if problems.nonEmpty then
    System.err.println(s"Sidebar checks failed, ${problems.length} issues found:")
    problems.foreach(problem => System.err.println(s"  - $problem"))
    System.err.println("Update the sidebar yaml or adjust ignore list in project/scripts/checkSidebarDocs.scala")
    sys.exit(1)
  else
    println("Sidebar checks passed.")
}

private def readSidebarEntries(sidebarPath: os.Path): Set[String] =
  os.read
    .lines(sidebarPath)
    .flatMap { line =>
      val trimmed = line.takeWhile(_ != '#')
      SidebarEntry
        .findFirstMatchIn(trimmed)
        .map(m => normalizePath(m.group(2)))
    }
    .toSet

private def missingReferencedPaths(sidebarPath: os.Path, sidebarEntries: Set[String], docsRoot: os.Path): Seq[String] =
  sidebarEntries
    .filterNot(path => os.exists(os.Path(path, docsRoot)))
    .toList
    .sorted
    .map(path => s"$sidebarPath references missing file: $path")

private def missingFromSidebar(
    label: String,
    docsDir: os.Path,
    docsRoot: os.Path,
    sidebarEntries: Set[String],
    ignored: Set[String]
): Seq[String] = {
  val markdownFiles = os
    .walk(docsDir)
    .filter(os.isFile)
    .filter(_.ext == "md")
    .map(path => normalizePath(path.relativeTo(docsRoot).toString))
    .toSet

  val missing = markdownFiles
    .filterNot(path => sidebarEntries.contains(path) || ignored.contains(path))
    .toList
    .sorted

  val reported = missing.map(path => s"$label markdown file not in sidebar: $path")

  val staleIgnores = ignored.filterNot(markdownFiles.contains).toList.sorted
  val ignoreWarnings = staleIgnores.map(path => s"$label ignored file not found on disk: $path")

  reported ++ ignoreWarnings
}

private def normalizePath(path: String): String =
  val stripped = path.trim.stripPrefix("./")
  val unquoted =
    if stripped.startsWith("\"") && stripped.endsWith("\"") then
      stripped.substring(1, stripped.length - 1)
    else if stripped.startsWith("'") && stripped.endsWith("'") then
      stripped.substring(1, stripped.length - 1)
    else stripped
  unquoted.replace('\\', '/')
