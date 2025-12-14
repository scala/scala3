//> using scala 3.8.0-RC3
//> using dep org.scala-lang::scala3-compiler:3.8.0-RC3
//> using dep com.vladsch.flexmark:flexmark-all:0.64.8
//> using toolkit default
//> using option -Wunused:all
//> using mainClass checkErrorCodeSnippets
// scalafmt: { maxColumn = 120 }

/** Non-reproducible error codes that cannot be triggered with pure Scala 3 source code.
  *
  * These error codes are used in the compiler but require special circumstances that cannot be reproduced with simple
  * source code (e.g., reading Scala 2 classfiles, specific bytecode patterns, runtime conditions).
  *
  * Documentation for these codes should still exist but uses `sc:nocompile` snippets. Tests will show warnings but pass
  * for these codes.
  *
  * TODO: These need manual review by the compiler team to find reproducible examples.
  *
  * Note: Error codes that have no message class should be marked `isActive = false` in ErrorMessageID.scala - those
  * don't need to be listed here.
  */
object NonReproducibleErrorCodes:
  val codes: Set[Int] = Set(
    25, // E025: IdentifierExpectedID - triggered by parser in specific contexts, needs investigation
    27, // E027: VarArgsParamMustComeLastID - triggered when varargs not last, needs correct syntax
    28, // E028: IllegalLiteralID - triggered by invalid literals, needs investigation
    33, // E033: PkgDuplicateSymbolID - requires package statements (not compatible with Scaladoc snippets)
    47, // E047: CyclicReferenceInvolvingImplicitID - triggered by implicit cycles, needs investigation
    98, // E098: FailureToEliminateExistential - only triggers when reading Scala 2 classfiles with existential types
    103, // E103: IllegalStartOfStatementID - requires top-level code (not compatible with Scaladoc snippets)
    105, // E105: TraitRedefinedFinalMethodFromAnyRefID - message class exists but never instantiated
    106, // E106: PackageNameAlreadyDefinedID - triggers in incremental compilation with classfile conflicts
    111, // E111: BadSymbolicReferenceID - requires missing classfiles on classpath
    113, // E113: SymbolHasUnparsableVersionNumberID - requires @migration annotation with invalid version
    114, // E114: SymbolChangedSemanticsInVersionID - requires @migration annotation with specific conditions
    138, // E138: TraitParameterUsedAsParentPrefixID - caught earlier by PostTyper with different message
    143, // E143: ErasedTypesCanOnlyBeFunctionTypesID - hard to reproduce,
    152 // E152: ExtensionCanOnlyHaveDefsID - never emitted
  )

  def isNonReproducible(errorCode: Int): Boolean = codes.contains(errorCode)
end NonReproducibleErrorCodes

object IgnoredInactiveErrorCodes:
  val codes: Set[Int] = Set(
    0 // E000: EmptyCatchOrFinallyBlockID - never used
  )
  def isIgnored(errorCode: Int): Boolean = codes.contains(errorCode)
end IgnoredInactiveErrorCodes

val Scala3_0_0_MaxErrorCode = 166 // CannotExtendFunctionID - the last error code in Scala 3.0.0

import scala.util.matching.Regex
import scala.util.CommandLineParser.FromString

import dotty.tools.dotc.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Comments.{ContextDoc, ContextDocstrings}
import dotty.tools.dotc.reporting.*
import dotty.tools.io.VirtualFile
import dotty.tools.dotc.config.Settings.ArgsSummary
import dotty.tools.dotc.config.{ScalaVersion, SpecificScalaVersion}

import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.ast.{Heading, FencedCodeBlock}
import com.vladsch.flexmark.util.ast.Node
import com.vladsch.flexmark.formatter.Formatter
import com.vladsch.flexmark.util.sequence.BasedSequence
import com.vladsch.flexmark.util.data.MutableDataSet

/** Logger trait for verbose output */
trait Logger:
  def debug(message: String): Unit
  def warn(message: String): Unit
  def info(message: String): Unit
  def error(message: String): Unit

object Logger:
  def console(isVerbose: Boolean): Logger = new Logger:
    def debug(message: String): Unit = if isVerbose then println(s"\u001b[90m$message${Console.RESET}")
    def info(message: String): Unit = println(message)
    def warn(message: String): Unit = println(s"${Console.YELLOW}$message${Console.RESET}")
    def error(message: String): Unit = println(s"${Console.RED}$message${Console.RESET}")

/** Check if an error code should be skipped based on its 'since' version */
def isFutureErrorCode(sinceVersion: Option[ScalaVersion]): Boolean =
  val currentCompilerVersion = getScriptCompilerVersion()
  sinceVersion.exists { sinceVer =>
    // Compare major.minor.rev only, treating RC/Milestone as part of the same version
    // e.g., 3.8.0-RC3 can test errors introduced in 3.8.0
    (currentCompilerVersion, sinceVer) match
      case (
            SpecificScalaVersion(currMaj, currMin, currRev, _),
            SpecificScalaVersion(sinceMaj, sinceMin, sinceRev, _)
          ) =>
        currMaj < sinceMaj ||
        (currMaj == sinceMaj && currMin < sinceMin) ||
        (currMaj == sinceMaj && currMin == sinceMin && currRev < sinceRev)
      case _ => currentCompilerVersion < sinceVer
  }

/** Check if an error code is inactive based on its 'until' version */
def isInactiveErrorCode(untilVersion: Option[ScalaVersion]): Boolean =
  lazy val currentCompilerVersion = getScriptCompilerVersion()
  untilVersion.exists { untilVer =>
    // Compare major.minor.rev only, treating RC/Milestone as part of the same version
    // If current version >= until version, the error code is inactive
    (currentCompilerVersion, untilVer) match
      case (
            dotty.tools.dotc.config.SpecificScalaVersion(currMaj, currMin, currRev, _),
            dotty.tools.dotc.config.SpecificScalaVersion(untilMaj, untilMin, untilRev, _)
          ) =>
        currMaj > untilMaj ||
        (currMaj == untilMaj && currMin > untilMin) ||
        (currMaj == untilMaj && currMin == untilMin && currRev >= untilRev)
      case _ => currentCompilerVersion >= untilVer
  }

/** Validates error code documentation snippets.
  *
  * Usage: scala --with-compiler checkErrorCodeSnippets.scala -- <path-to-error-code.md> --verbose
  *
  * Validates that:
  *   - Example snippets fail with the expected error code
  *   - Error section matches actual compiler output
  *   - Solution snippets compile without warnings
  *
  * Arguments: mdFile Path to error code markdown file verbose Pass "verbose" or "true" to print all compiler outputs
  */
@main def checkErrorCodeSnippets(
    mdFile: os.Path,
    varArgs: String*
): Unit =
  val isVerbose = varArgs.contains("--verbose")
  val updateOutput = varArgs.contains("--update-output")

  // Create the appropriate log based on verbose mode
  given log: Logger = Logger.console(isVerbose = isVerbose)

  val fileName = mdFile.last
  val expectedErrorCode = extractErrorCode(fileName).getOrElse {
    log.error(s"Error: Could not extract error code from filename: $fileName")
    sys.exit(1)
  }
  val expectedErrorCodeStr = formatErrorCode(expectedErrorCode)
  log.info(s"Checking error code documentation: $fileName")
  log.debug(s"Expected error code: $expectedErrorCodeStr")
  log.info("=" * 60)

  val content = os.read(mdFile)
  val parsed = parseMarkdown(content)

  // Check if error code is from a newer compiler version
  if isFutureErrorCode(parsed.sinceVersion) then
    val currentCompilerVersion = getScriptCompilerVersion()
    log.warn(
      s"⚠ Skipping validation: Error code introduced in ${parsed.sinceVersion.get.unparse}, " +
        s"current compiler is ${currentCompilerVersion.unparse}"
    )
    log.info("  Documentation will be validated once the error code is available in the released compiler")
    sys.exit(0)

  // Check if error code is inactive (was removed in a previous compiler version)
  if isInactiveErrorCode(parsed.untilVersion) then
    val currentCompilerVersion = getScriptCompilerVersion()
    log.warn(
      s"⚠ Skipping validation: Error code was made inactive in ${parsed.untilVersion.get.unparse}, " +
        s"current compiler is ${currentCompilerVersion.unparse}"
    )
    log.info("  This error code is no longer emitted by the compiler")
    sys.exit(0)

  var hasErrors = false

  // Check example snippets - they should fail
  log.info("\n[1/3] Checking Example snippets (should fail with expected error)...")

  parsed.exampleSnippets.zipWithIndex.foreach { case (snippet, idx) =>
    val result = checkExampleSnippet(snippet, expectedErrorCode, idx + 1, collectDiagnostics = updateOutput)
    if !result then hasErrors = true
  }

  log.info(s"\n[2/3] Validating Error section output...")
  // Handle non-reproducible error codes
  val isNonReproducible = NonReproducibleErrorCodes.isNonReproducible(expectedErrorCode)

  // Handle update-output mode
  if parsed.exampleSnippets.isEmpty && parsed.exampleNoCompileSnippets.isEmpty then
    if isNonReproducible then
      log.warn(s"  ⚠ $expectedErrorCodeStr is marked as non-reproducible in NonReproducibleErrorCodes.codes")
      log.warn(s"    No example snippets expected for this error code")
    else
      log.error(s"  ✗ No example snippets found to verify")
      hasErrors = true
  else if parsed.exampleSnippets.isEmpty && parsed.exampleNoCompileSnippets.nonEmpty then
    if isNonReproducible then
      log.warn(s"  ⚠ $expectedErrorCodeStr is marked as non-reproducible in NonReproducibleErrorCodes.codes")
    else
      log.warn(s"  ⚠ All example snippets are marked 'sc:nocompile' - cannot verify error code")
      log.warn(s"    Consider adding this error to NonReproducibleErrorCodes.codes if it cannot be reproduced")
  else if updateOutput then
    log.info(s"\n Updating Error section output...")
    val snippet = parsed.exampleSnippets.head // Use first example snippet
    val result = compileSnippet(snippet)

    // Verify compilation produced the expected error code before updating
    if !result.errorCodes.contains(expectedErrorCode) then
      log.error(s"  ✗ Cannot update: compilation did not produce expected error code $expectedErrorCodeStr")
      log.error(s"    Found error codes: ${result.errorCodes.map(formatErrorCode).mkString(", ")}")
      log.debug(s"    Messages:\n${result.messages.map("      " + _).mkString("\n")}")
      hasErrors = true
    else
      val newErrorOutput = getFormattedOutputForComparison(result.diagnostics)
      val updatedContent = updateErrorSection(content, newErrorOutput)
      os.write.over(mdFile, updatedContent)
      log.info(s"  ✓ Updated error section in $mdFile")
  else // check only
    log.info(s"\nChecking Error section output...")
    log.debug(s"    Documented error output:\n${parsed.errorOutput}")
    val errorCodePattern = s"\\[${Regex.quote(expectedErrorCodeStr)}\\]".r

    // For non-reproducible error codes, skip output validation (error code may not be in released compiler)
    val hasCustomSnippets = parsed.exampleSnippets.exists(_.options.contains("custom-sc:fail"))
    if isNonReproducible && hasCustomSnippets then
      log.warn(s"  ⚠ $expectedErrorCodeStr is non-reproducible - skipping error output validation")
      log.warn(s"    Error output was manually verified when documentation was created")
    else if errorCodePattern.findFirstIn(parsed.errorOutput).isEmpty then
      log.error(s"  ✗ Error section does not contain expected error code $expectedErrorCodeStr")
      hasErrors = true
    else
      log.info(s"  ✓ Error section contains expected error code $expectedErrorCodeStr")
      // Collect all diagnostics from example snippets
      val allDiagnostics = parsed.exampleSnippets.flatMap { snippet =>
        compileSnippet(snippet).diagnostics
      }
      val actualOutput = getFormattedOutputForComparison(allDiagnostics)
      log.debug(s"    Actual compiler output:\n$actualOutput")

      // Exact match comparison
      compareOutputs(parsed.errorOutput, actualOutput) match
        case None =>
          log.info(s"  ✓ Error section matches compiler output exactly")
        case Some(diff) =>
          log.error(s"  ✗ Error section does not match compiler output")
          log.info(s"    Diff (- documented, + actual):\n$diff")
          log.info(
            s"\n    Update with: scala project/scripts/checkErrorCodeSnippets.scala --with-compiler -- $mdFile --update-output"
          )
          hasErrors = true

  // Check solution snippets - they should compile without warnings
  log.info("\n[3/3] Checking Solution snippets (should compile without warnings)...")
  parsed.solutionSnippets.zipWithIndex.foreach { case (snippet, idx) =>
    val result = checkSolutionSnippet(snippet, idx + 1)
    if !result then hasErrors = true
  }

  if parsed.solutionSnippets.isEmpty then
    if isNonReproducible then log.warn(s"  ⚠ No solution snippets found (acceptable for non-reproducible error code)")
    else log.warn("  ⚠ No solution snippets found with 'sc:compile' or 'custom-sc:compile' marker")

  log.info("\n" + "=" * 60)
  if hasErrors then
    log.error("FAILED: Some checks did not pass")
    sys.exit(1)
  else
    log.info(s"${Console.GREEN}SUCCESS: All checks passed${Console.RESET}")
    sys.exit(0)
end checkErrorCodeSnippets

/** Updates error outputs for all error code documentation files.
  *
  * Usage: scala --with-compiler checkErrorCodeSnippets.scala --main-class updateAllErrorCodeOutputs
  */
@main def updateAllErrorCodeOutputs(varArgs: String*): Unit =
  val isVerbose = varArgs.contains("--verbose")

  given log: Logger = Logger.console(isVerbose = isVerbose)

  val errorCodesDir = os.pwd / "docs" / "_docs" / "reference" / "error-codes"

  if !os.exists(errorCodesDir) then
    log.error(s"Error codes directory not found: $errorCodesDir")
    sys.exit(1)

  val errorCodeFiles = os
    .list(errorCodesDir)
    .filter(_.last.matches("E\\d+\\.md"))
    .sorted

  log.info(s"Found ${errorCodeFiles.size} error code documentation files")
  log.info("=" * 60)

  var updated = 0
  var skipped = 0
  var failed = 0

  errorCodeFiles.foreach { mdFile =>
    val fileName = mdFile.last
    extractErrorCode(fileName) match
      case None =>
        log.warn(s"  ⚠ Skipping $fileName: could not extract error code")
        skipped += 1

      case Some(expectedErrorCode) =>
        val expectedErrorCodeStr = formatErrorCode(expectedErrorCode)
        log.info(s"\n$expectedErrorCodeStr: $fileName")

        val content = os.read(mdFile)
        val parsed = parseMarkdown(content)

        if isInactiveErrorCode(parsed.untilVersion) then
          log.warn(s"  ⚠ Skipping: error code is inactive")
          skipped += 1
        else if isFutureErrorCode(parsed.sinceVersion) then
          log.warn(s"  ⚠ Skipping: error code is future")
          skipped += 1
        else if parsed.exampleSnippets.isEmpty then
          log.warn(s"  ⚠ Skipping: no example snippets found")
          skipped += 1
        else
          val snippet = parsed.exampleSnippets.head
          val result = compileSnippet(snippet)

          if !result.errorCodes.contains(expectedErrorCode) then
            log.error(s"  ✗ Compilation did not produce expected error code $expectedErrorCodeStr")
            log.error(s"    Found: ${result.errorCodes.map(formatErrorCode).mkString(", ")}")
            failed += 1
          else
            val newErrorOutput = getFormattedOutputForComparison(result.diagnostics)
            val currentOutput = parsed.errorOutput

            // Check if update is needed
            compareOutputs(currentOutput, newErrorOutput) match
              case None =>
                log.info(s"  ✓ Already up to date")
                skipped += 1
              case Some(_) =>
                val updatedContent = updateErrorSection(content, newErrorOutput)
                os.write.over(mdFile, updatedContent)
                log.info(s"  ✓ Updated")
                updated += 1
  }

  log.info("\n" + "=" * 60)
  log.info(s"Summary: $updated updated, $skipped skipped, $failed failed")

  if failed > 0 then
    log.error("Some files could not be updated")
    sys.exit(1)
  else log.info(s"${Console.GREEN}Done${Console.RESET}")
end updateAllErrorCodeOutputs

given FromString[os.Path] = os.Path(_, os.pwd)

case class Snippet(code: String, options: List[String])

case class ParsedMarkdown(
    exampleSnippets: List[Snippet],
    exampleNoCompileSnippets: List[Snippet],
    errorOutput: String,
    solutionSnippets: List[Snippet],
    sinceVersion: Option[ScalaVersion],
    untilVersion: Option[ScalaVersion]
)

def extractErrorCode(fileName: String): Option[Int] =
  val pattern = """E(\d+)\.md""".r
  fileName match
    case pattern(num) => Some(num.toInt)
    case _            => None

def formatErrorCode(code: Int): String = f"E$code%03d"

/** Extract the 'since' version from YAML frontmatter */
def extractSinceVersion(content: String): Option[ScalaVersion] =
  val yamlPattern = """(?s)^---\n(.*?)\n---""".r
  yamlPattern.findFirstMatchIn(content).flatMap { m =>
    val yaml = m.group(1)
    val sincePattern = """since:\s*([0-9.]+(?:-[A-Za-z0-9]+)?)""".r
    sincePattern.findFirstMatchIn(yaml).flatMap { versionMatch =>
      val versionStr = versionMatch.group(1)
      ScalaVersion.parse(versionStr).toOption
    }
  }

/** Extract the 'until' version from YAML frontmatter */
def extractUntilVersion(content: String): Option[ScalaVersion] =
  val yamlPattern = """(?s)^---\n(.*?)\n---""".r
  yamlPattern.findFirstMatchIn(content).flatMap { m =>
    val yaml = m.group(1)
    val untilPattern = """until:\s*([0-9.]+(?:-[A-Za-z0-9]+)?)""".r
    untilPattern.findFirstMatchIn(yaml).flatMap { versionMatch =>
      val versionStr = versionMatch.group(1)
      ScalaVersion.parse(versionStr).toOption
    }
  }

/** Get the compiler version used by this script (from //> using scala directive) */
def getScriptCompilerVersion(): ScalaVersion =
  // The script uses //> using scala 3.7.4 and //> using dep org.scala-lang::scala3-compiler:3.7.4
  // We extract it from the compiler artifact version
  val compilerVersion = dotty.tools.dotc.config.Properties.versionNumberString
  ScalaVersion.parse(compilerVersion).getOrElse(ScalaVersion.current)

/** Run scala compile --server=false on code and capture output */
def runScalaCompile(code: String): String =
  val compilerVersion = scala.util.Properties.versionNumberString match {
    case s"2.${_}" => "3.8.0-RC3"
    case version   => version
  }
  os.proc(
    "scala",
    "compile",
    "--server=false",
    s"--scala-version=$compilerVersion",
    "-color:never",
    "-explain",
    "-explain-types",
    "-explain-cyclic",
    "_"
  ).call(stderr = os.Pipe, mergeErrIntoOut = true, stdin = code.getBytes("UTF-8"), check = false)
    .out
    .text()

/** Format scala compile output for documentation */
def formatScalaCompileOutput(compileOutput: String): String =
  stripAnsi(compileOutput)
    .replaceAll(raw"(/[^:\s]+(?:/[^:\s]+)*\.scala)", "example.scala")
    .linesIterator
    .takeWhile(!_.matches(raw"^\d+ error(s)? found"))
    .mkString("\n")

/** Format compiler output for documentation, normalizing paths */
def formatCompilerOutput(diagnostic: DiagnosticMessage, expectedErrorCodeStr: String): String =
  val lines = diagnostic.message.linesIterator.toList

  // Format the first line with the error code, keeping original positions but normalizing file path
  val formattedLines = lines match
    case firstLine :: rest =>
      // Replace the file path with example.scala but keep line:col positions
      val formattedFirst = firstLine.replaceAll("/.*/[^/]*\\.scala", "example.scala")
      s"-- [$expectedErrorCodeStr] $formattedFirst" :: rest
    case Nil =>
      throw new RuntimeException("No compiler output received for error formatting")

  formattedLines.mkString("\n")

/** Update the error section in markdown content */
def updateErrorSection(content: String, newErrorOutput: String): String =
  // Preserve YAML front matter - flexmark modifies it during rendering
  val yamlPattern = """(?s)^(---\n.*?\n---\n)(.*)$""".r
  val (yamlFrontMatter, bodyContent) = content match
    case yamlPattern(yaml, body) => (Some(yaml), body)
    case _                       => (None, content)

  // Configure formatter to preserve whitespace in fenced code blocks
  val options = new MutableDataSet()
  options.set(Formatter.FENCED_CODE_MINIMIZE_INDENT, java.lang.Boolean.FALSE)
  options.set(Formatter.INDENTED_CODE_MINIMIZE_INDENT, java.lang.Boolean.FALSE)

  val parser = Parser.builder(options).build()
  val formatter = Formatter.builder(options).build()
  val document = parser.parse(bodyContent)

  // Find: ### Error or ### Warning
  var n: Node | Null = document.getFirstChild
  while n != null do
    n match
      case h: Heading
          if h.getLevel == 3 && (h.getText.toString.trim == "Error" || h.getText.toString.trim == "Warning") =>
        // Scan forward until next heading of level <= 3, looking for fenced sc:nocompile
        var m: Node | Null = h.getNext
        var updated = false
        while m != null && !updated do
          m match
            case hh: Heading if hh.getLevel <= 3 =>
              // stop scanning this section
              updated = true // "done", but without changing anything
            case cb: FencedCodeBlock if cb.getInfo.toString.contains("sc:nocompile") =>
              // Update the content of the fenced code block
              val seq = BasedSequence.of(newErrorOutput + "\n")
              cb.setContent(seq, java.util.List.of(seq))
              updated = true
            case _ =>
              m = m.getNext
        n = h.getNext // continue after heading
      case _ =>
        n = n.getNext

  val renderedBody = formatter.render(document)
  yamlFrontMatter.fold(renderedBody)(_ + renderedBody)

def parseMarkdown(content: String): ParsedMarkdown =
  val parser = Parser.builder().build()
  val document = parser.parse(content)

  var currentSection: Option["example" | "output" | "solution"] = None
  var exampleSnippets = List.empty[Snippet]
  var exampleNoCompileSnippets = List.empty[Snippet]
  var errorOutput = ""
  var solutionSnippets = List.empty[Snippet]

  // Extract since and until versions from YAML frontmatter
  val sinceVersion = extractSinceVersion(content)
  val untilVersion = extractUntilVersion(content)

  // Iterate through all nodes in document order
  var node: Node = document.getFirstChild
  while node != null do
    node match
      case heading: Heading =>
        val text = heading.getText.toString.toLowerCase
        val level = heading.getLevel
        currentSection = (level, text) match
          case (2, "example")           => Some("example")
          case (3, "error" | "warning") => Some("output")
          case (3, "solution")          => Some("solution")
          case (1 | 2, _)               => None // Reset on new top-level sections
          case _                        => currentSection // Keep current section for other headings

      case codeBlock: FencedCodeBlock =>
        val info = codeBlock.getInfo.toString.trim
        val code = codeBlock.getContentChars.toString.trim
        val parts = info.split("\\s+").toList
        // Parse both sc: and custom-sc: options
        val options = parts.filter(p => p.startsWith("sc:") || p.startsWith("sc-") || p.startsWith("custom-sc:"))

        // custom-sc: attributes override sc:nocompile for our snippet compiler
        // This allows snippets to be ignored by Scaladoc but still tested by us
        currentSection match
          case Some("example") if options.contains("sc:fail") || options.contains("custom-sc:fail") =>
            exampleSnippets = exampleSnippets :+ Snippet(code, options)
          case Some("example") if options.contains("sc:nocompile") =>
            exampleNoCompileSnippets = exampleNoCompileSnippets :+ Snippet(code, options)
          case Some("output") =>
            errorOutput = code
          case Some("solution") if options.contains("sc:compile") || options.contains("custom-sc:compile") =>
            solutionSnippets = solutionSnippets :+ Snippet(code, options)
          case _ => // ignore other code blocks

      case _ => // ignore other node types

    node = node.getNext
  end while

  ParsedMarkdown(exampleSnippets, exampleNoCompileSnippets, errorOutput, solutionSnippets, sinceVersion, untilVersion)
end parseMarkdown

def extractScalacOptions(options: List[String]): List[String] =
  options
    .filter(_.startsWith("sc-opts:"))
    .flatMap(_.stripPrefix("sc-opts:").split(",").toList)

/** Strip ANSI escape codes from a string */
def stripAnsi(s: String): String =
  s.replaceAll("\u001b\\[[0-9;]*m", "")

/** Messages to filter out when comparing outputs (added by -Werror compilation) */
val filteredMessages: Set[String] = Set(
  "No warnings can be incurred under -Werror"
)

/** Get the formatted compiler output for comparison, filtering out -Werror messages */
def getFormattedOutputForComparison(diagnostics: List[DiagnosticMessage]): String =
  diagnostics
    .filterNot(d => filteredMessages.exists(d.message.contains))
    .map(_.formattedOutput)
    .mkString("\n")
    .trim

/** Normalize whitespace for comparison (trim lines, normalize line endings) */
def normalizeWhitespace(s: String): String =
  s.linesIterator
    .map(_.stripTrailing())
    .mkString("\n")
    .trim

/** Compare documented error output with actual compiler output. Returns None if they match, or Some(diff) with the
  * differences.
  */
def compareOutputs(documented: String, actual: String): Option[String] =
  val normalizedDocumented = normalizeWhitespace(documented)
  val normalizedActual = normalizeWhitespace(actual)

  if normalizedDocumented == normalizedActual then None
  else
    // Generate a simple line-by-line diff
    val docLines = normalizedDocumented.linesIterator.toVector
    val actLines = normalizedActual.linesIterator.toVector
    val maxLines = math.max(docLines.length, actLines.length)

    val diffLines = (0 until maxLines).flatMap { i =>
      val docLine = docLines.lift(i)
      val actLine = actLines.lift(i)
      (docLine, actLine) match
        case (Some(d), Some(a)) if d == a =>
          Seq(s"  $d")
        case (Some(d), Some(a)) =>
          Seq(s"${Console.RED}- $d${Console.RESET}", s"${Console.GREEN}+ $a${Console.RESET}")
        case (Some(d), None) =>
          Seq(s"${Console.RED}- $d${Console.RESET}")
        case (None, Some(a)) =>
          Seq(s"${Console.GREEN}+ $a${Console.RESET}")
        case (None, None) =>
          Seq.empty
    }
    Some(diffLines.mkString("\n"))

/** A diagnostic message with its associated error code and formatted output */
case class DiagnosticMessage(errorCode: Option[Int], message: String, formattedOutput: String)

/** Result of compiling a snippet */
case class CompileResult(
    hasErrors: Boolean,
    hasWarnings: Boolean,
    diagnostics: List[DiagnosticMessage]
):
  def errorCodes: Set[Int] = diagnostics.flatMap(_.errorCode).toSet
  def messages: List[String] = diagnostics.map(_.message)

  /** Get full formatted output like scala compile produces */
  def formattedOutput: String = diagnostics.map(_.formattedOutput).mkString("\n")

/** Helper for formatting diagnostics like console output */
object DiagnosticFormatter extends MessageRendering

/** Snippet compiler with dedicated output directory */
object SnippetCompiler:
  // Dedicated temp directory for compilation outputs (cached to avoid creating many dirs)
  private lazy val outputDir = os.temp.dir(prefix = "snippet-compile-")

  // Base options used for all compilations
  private val baseOpts = List("-usejavacp", "-color:never", "-explain")

  def compile(code: String, extraOpts: List[String]): CompileResult =
    // Create fresh compiler infrastructure for each compilation
    // (ContextBase and Compiler cannot be reused due to stale symbol issues)
    val base = new ContextBase
    val compiler = new Compiler

    // Create fresh reporter
    val reporter = new StoreReporter(null) with UniqueMessagePositions with HideNonSensicalMessages

    // Create virtual source file
    val virtualFile = VirtualFile("snippet.scala", code.getBytes("UTF-8"))
    val sourceFile = dotty.tools.dotc.util.SourceFile(virtualFile, scala.io.Codec.UTF8)

    // Process all options
    val allOpts = baseOpts ++ List("-d", outputDir.toString) ++ extraOpts
    val ArgsSummary(sstate, _, _, _) =
      base.initialCtx.settings.processArguments(allOpts, processAll = true, base.initialCtx.settingsState)

    given ctx: Context = base.initialCtx.fresh
      .setReporter(reporter)
      .setSettings(sstate)
      .setProperty(ContextDoc, new ContextDocstrings)

    // Compile
    val run = compiler.newRun
    run.compileSources(List(sourceFile))

    // Extract results
    val runCtx = run.runContext
    val rawMessages = reporter.removeBufferedMessages(using runCtx)

    val diagnostics = rawMessages.map { diag =>
      val errorNumber = diag.msg.errorId.errorNumber
      val errorCode = Option.when(errorNumber >= 0)(errorNumber)
      // Get the raw message and the formatted console-style output
      val rawMessage = stripAnsi(diag.msg.message)
      val formattedOutput = stripAnsi(DiagnosticFormatter.messageAndPos(diag)(using runCtx))
        .replaceAll("snippet\\.scala", "example.scala") // Normalize file name
      DiagnosticMessage(errorCode, rawMessage, formattedOutput)
    }.toList

    CompileResult(
      hasErrors = reporter.hasErrors,
      hasWarnings = reporter.hasWarnings,
      diagnostics = diagnostics
    )
  end compile
end SnippetCompiler

/** Compile a snippet using the Scala 3 compiler */
def compileSnippet(snippet: Snippet): CompileResult =
  val extraOpts = extractScalacOptions(snippet.options)
  try SnippetCompiler.compile(snippet.code, extraOpts)
  catch
    case exception: Exception =>
      System.err.println(s"Crash when compiling snippet:")
      System.err.println(snippet.code)
      exception.printStackTrace()
      throw exception

def checkExampleSnippet(snippet: Snippet, expectedErrorCode: Int, index: Int, collectDiagnostics: Boolean = false)(using
    log: Logger
): Boolean =
  print(s"  Example #$index: ")

  log.debug("")
  log.debug("    Snippet code:")
  log.debug("    ```scala")
  snippet.code.linesIterator.foreach(line => log.debug(s"    $line"))
  log.debug("    ```")
  if snippet.options.nonEmpty then log.debug(s"    Options: ${snippet.options.mkString(", ")}")

  val result = compileSnippet(snippet)
  val isCustomSnippet = snippet.options.contains("custom-sc:fail")
  val isNonReproducible = NonReproducibleErrorCodes.isNonReproducible(expectedErrorCode)

  log.debug("    Compiler output:")
  result.diagnostics.foreach(d => d.formattedOutput.linesIterator.foreach(line => log.debug(s"      $line")))

  if collectDiagnostics then
    // In collect diagnostics mode, just check that compilation fails and print basic status
    val success = result.hasErrors || result.hasWarnings
    if success then log.info(s"✓ Compilation failed (collecting diagnostics)")
    else log.error(s"✗ Expected compilation to fail, but it succeeded")
    success
  else if isCustomSnippet && isNonReproducible then
    // For custom snippets with non-reproducible error codes, just verify compilation produces warnings/errors
    // The error code may not be available in the released compiler version
    if !result.hasErrors && !result.hasWarnings then
      log.error("✗ Expected compilation to fail, but it succeeded")
      false
    else
      log.info(s"✓ Compilation produces warnings/errors (non-reproducible error code, skipping code validation)")
      true
  else
    // Normal validation mode
    if !result.hasErrors && !result.hasWarnings then
      log.error("✗ Expected compilation to fail, but it succeeded")
      false
    else
      def unexpectedCodes = result.errorCodes - expectedErrorCode
      if !result.errorCodes.contains(expectedErrorCode) then
        log.error(s"✗ Expected error code ${formatErrorCode(expectedErrorCode)} not found in output")
        log.error(s"    Found error codes: ${result.errorCodes.map(formatErrorCode).mkString(", ")}")
        log.debug(s"    Messages:\n${result.messages.map("      " + _).mkString("\n")}")
        false
      else if unexpectedCodes.nonEmpty then
        log.error(s"✗ Found unexpected error codes: ${unexpectedCodes.map(formatErrorCode).mkString(", ")}")
        log.error(s"    Expected only: ${formatErrorCode(expectedErrorCode)}")
        false
      else
        log.info(s"✓ Fails with expected error code ${formatErrorCode(expectedErrorCode)}")
        true
end checkExampleSnippet

def checkSolutionSnippet(snippet: Snippet, index: Int)(using log: Logger): Boolean =
  log.info(s"  Solution #$index: ")

  log.debug("")
  log.debug("    Snippet code:")
  log.debug("    ```scala")
  snippet.code.linesIterator.foreach(line => log.debug(s"    $line"))
  log.debug("    ```")
  if snippet.options.nonEmpty then log.debug(s"    Options: ${snippet.options.mkString(", ")}")

  // Always compile solutions with -Werror to catch warnings
  val snippetWithWerror = snippet.copy(
    options = snippet.options.filterNot(_.contains("-Werror")) :+ "sc-opts:-Werror"
  )

  val result = compileSnippet(snippetWithWerror)

  val outputLabel = if result.diagnostics.isEmpty then "(none)" else ""
  log.debug(s"    Compiler output: $outputLabel")
  result.diagnostics.foreach(d => d.formattedOutput.linesIterator.foreach(line => log.debug(s"      $line")))

  if !result.hasErrors && !result.hasWarnings then
    log.info("✓ Compiles successfully without warnings")
    true
  else
    log.error("✗ Failed to compile or has warnings")
    log.debug(s"    Messages:\n${result.messages.map("      " + _).mkString("\n")}")
    false
end checkSolutionSnippet
