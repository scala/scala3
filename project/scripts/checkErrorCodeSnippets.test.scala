//> using file checkErrorCodeSnippets.scala
// scalafmt: { maxColumn = 120 }

import scala.util.matching.Regex
import dotty.tools.dotc.reporting.ErrorMessageID
import dotty.tools.dotc.config.{ScalaVersion, SpecificScalaVersion}

class ErrorCodeSnippetsTest extends munit.FunSuite:

  // Check if an error code should be skipped based on its 'since' version
  def isFutureErrorCode(sinceVersion: Option[ScalaVersion]): Boolean =
    val currentCompilerVersion = getScriptCompilerVersion()
    sinceVersion.exists { sinceVer =>
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

  // Check if an error code is inactive based on its 'until' version
  def isInactiveErrorCode(untilVersion: Option[ScalaVersion]): Boolean =
    val currentCompilerVersion = getScriptCompilerVersion()
    untilVersion.exists { untilVer =>
      // If current version >= until version, the error code is inactive
      (currentCompilerVersion, untilVer) match
        case (
              SpecificScalaVersion(currMaj, currMin, currRev, _),
              SpecificScalaVersion(untilMaj, untilMin, untilRev, _)
            ) =>
          currMaj > untilMaj ||
          (currMaj == untilMaj && currMin > untilMin) ||
          (currMaj == untilMaj && currMin == untilMin && currRev >= untilRev)
        case _ => currentCompilerVersion >= untilVer
    }

  /** Helper to check assumptions and print skip reason if assumption fails */
  def assumeWithLogging(condition: Boolean, message: => String): Unit =
    if !condition then
      println(s"${Console.YELLOW}  ⚠ Skipping: $message${Console.RESET}")
      assume(false, message)

  // Find all error code documentation files
  val errorCodesDir: os.Path = os.pwd / "docs" / "_docs" / "reference" / "error-codes"

  val errorCodeFiles: Seq[os.Path] =
    if !os.exists(errorCodesDir) then sys.error(s"Error codes directory not found: $errorCodesDir")
    os.list(errorCodesDir)
      .filter(_.last.matches("E\\d+\\.md"))
      .sorted

  // Get all error message IDs from the compiler
  val allErrorIds: Seq[ErrorMessageID] =
    ErrorMessageID.values.toSeq.filterNot(_ == ErrorMessageID.NoExplanationID)

  val activeErrorIds: Seq[ErrorMessageID] = allErrorIds.filter(_.isActive)
  val inactiveErrorIds: Seq[ErrorMessageID] = allErrorIds.filterNot(_.isActive)

  // Extract documented error codes from file names (as Int)
  val documentedErrorCodes: Map[Int, os.Path] =
    errorCodeFiles
      .flatMap: f =>
        extractErrorCode(f.last).map: errorCode =>
          (errorCode, f)
      .toMap

  val (inactiveDocumentedErrorCodes, activeDocumentedErrorCodes) = documentedErrorCodes.partition { (errorCode, _) =>
    inactiveErrorIds.exists(_.errorNumber == errorCode)
  }

  extension (id: ErrorMessageID)
    def isIgnored: Boolean =
      inactiveErrorIds.contains(id.errorNumber) || IgnoredInactiveErrorCodes.isIgnored(id.errorNumber)

  // Test: All active error codes should have documentation
  test("All active error codes must have documentation") {
    val missingDocs = activeErrorIds.filterNot { id =>
      documentedErrorCodes.contains(id.errorNumber)
    }

    if missingDocs.nonEmpty then
      val missing = missingDocs.map(id => s"${formatErrorCode(id.errorNumber)} (${id.toString})").mkString("\n  - ")
      fail(s"Missing documentation for active error codes:\n  - $missing")
  }

  test("Inactive error codes must have documentation") {
    val missingDocs = inactiveErrorIds.filterNot { id =>
      documentedErrorCodes.contains(id.errorNumber) || IgnoredInactiveErrorCodes.isIgnored(id.errorNumber)
    }
    if missingDocs.nonEmpty then
      val missing = missingDocs.map(id => s"${formatErrorCode(id.errorNumber)} (${id.toString})").mkString("\n  - ")
      fail(s"Missing documentation for inactive error codes:\n  - $missing")

  }
  inactiveDocumentedErrorCodes.foreach: (errorCode, mdFile) =>
    test(s"${formatErrorCode(errorCode)} inactive has 'until' version") {
      os.read(mdFile).linesIterator.find(_.startsWith("until: ")) match {
        case Some(s"until: $version") =>
          assert(ScalaVersion.parse(version).isSuccess, s"Invalid until version: $version")
        case None       => fail(s"No until version found in $mdFile")
        case Some(line) => fail(s"Unexpected line in $mdFile: $line")
      }
    }

  test("Documentation files must correspond to valid error codes") {
    val validErrorCodes = allErrorIds.map(_.errorNumber).toSet

    // Filter out error codes that have a 'since' version higher than current compiler
    val invalidDocs = documentedErrorCodes.filterNot { (errorCode, mdFile) =>
      validErrorCodes.contains(errorCode) || {
        // Check if this error code has a 'since' version that's newer than current compiler
        os.exists(mdFile) && {
          val content = os.read(mdFile)
          val parsed = parseMarkdown(content)
          isFutureErrorCode(parsed.sinceVersion)
        }
      }
    }

    if invalidDocs.nonEmpty then
      val invalid = invalidDocs.keys.toSeq.sorted.map(formatErrorCode).mkString(", ")
      fail(s"Found documentation for non-existent error codes: $invalid")
  }

  test("All documented error codes must be listed in sidebar.yml") {
    val sidebarPath = os.pwd / "docs" / "sidebar.yml"
    assert(os.exists(sidebarPath), s"sidebar.yml not found at $sidebarPath")

    val sidebarContent = os.read(sidebarPath)

    // Extract error codes from sidebar.yml entries like "- page: reference/error-codes/E001.md"
    val sidebarErrorCodePattern = """page:\s*reference/error-codes/E(\d+)\.md""".r
    val sidebarErrorCodes: Set[Int] = sidebarErrorCodePattern
      .findAllMatchIn(sidebarContent)
      .map(_.group(1).toInt)
      .toSet

    // Check that all documented error codes are in the sidebar
    val missingFromSidebar = documentedErrorCodes.keys.toSet -- sidebarErrorCodes
    if missingFromSidebar.nonEmpty then
      val missing = missingFromSidebar.toSeq.sorted.map(formatErrorCode).mkString("\n  - ")
      fail(s"Error codes documented but missing from docs/sidebar.yml:\n  - $missing")

    // Check that all sidebar entries have corresponding documentation files
    val missingDocs = sidebarErrorCodes -- documentedErrorCodes.keys.toSet
    if missingDocs.nonEmpty then
      val missing = missingDocs.toSeq.sorted.map(formatErrorCode).mkString("\n  - ")
      fail(s"Error codes in docs/sidebar.yml but missing documentation files:\n  - $missing")
  }

  // Generate two tests for each error code file
  errorCodeFiles.foreach { mdFile =>
    val fileName = mdFile.last
    extractErrorCode(fileName).foreach { errorCode =>
      val errorCodeStr = formatErrorCode(errorCode)

      // Test 1: Check example snippets fail with expected error
      test(s"$errorCodeStr - examples must fail with expected error") {
        val content = os.read(mdFile)
        val parsed = parseMarkdown(content)

        // Skip validation for error codes from future compiler versions
        assumeWithLogging(
          !isFutureErrorCode(parsed.sinceVersion),
          s"$errorCodeStr introduced in ${parsed.sinceVersion.get.unparse}, current compiler is ${getScriptCompilerVersion().unparse}"
        )

        // Skip validation for inactive error codes
        assumeWithLogging(
          !isInactiveErrorCode(parsed.untilVersion),
          s"$errorCodeStr was made inactive in ${parsed.untilVersion.get.unparse}, current compiler is ${getScriptCompilerVersion().unparse}"
        )

        // Skip validation for non-reproducible error codes
        assumeWithLogging(
          !NonReproducibleErrorCodes.isNonReproducible(errorCode),
          s"$errorCodeStr is non-reproducible - cannot be triggered with pure Scala 3 source code"
        )

        // If all example snippets are sc:nocompile, skip validation with a note
        assumeWithLogging(
          parsed.exampleSnippets.nonEmpty || parsed.exampleNoCompileSnippets.isEmpty,
          s"$errorCodeStr has only sc:nocompile snippets - requires special circumstances"
        )

        assert(parsed.exampleSnippets.nonEmpty, s"No example snippets found in $mdFile")

        // Collect all diagnostics from compiling example snippets
        val allDiagnostics = parsed.exampleSnippets.zipWithIndex.flatMap { case (snippet, idx) =>
          val result = compileSnippet(snippet)

          assert(
            result.hasErrors || result.hasWarnings,
            s"Example #${idx + 1} should fail compilation but succeeded"
          )

          assert(
            result.errorCodes.contains(errorCode),
            s"Example #${idx + 1} should emit error code $errorCodeStr.\nFound: ${result.errorCodes.map(formatErrorCode).mkString(", ")}\nMessages:\n${result.messages.mkString("\n")}"
          )

          // Check for unexpected error codes
          val unexpectedCodes = result.errorCodes - errorCode
          assert(
            unexpectedCodes.isEmpty,
            s"Example #${idx + 1} emitted unexpected error codes: ${unexpectedCodes.map(formatErrorCode).mkString(", ")}. Expected only $errorCodeStr"
          )

          result.diagnostics
        }

        // Check error section contains expected error code
        assert(parsed.errorOutput.nonEmpty, s"No error output found in $mdFile")
        val errorCodePattern = s"\\[${Regex.quote(errorCodeStr)}\\]".r
        assert(
          errorCodePattern.findFirstIn(parsed.errorOutput).isDefined,
          s"Error section should contain error code $errorCodeStr"
        )

        // Exact match comparison of documented vs actual compiler output
        val actualOutput = getFormattedOutputForComparison(allDiagnostics)

        compareOutputs(parsed.errorOutput, actualOutput) match
          case None       => // Output matches exactly
          case Some(diff) =>
            fail(
              s"""Error section does not match compiler output exactly.
                 |
                 |Diff (- documented, + actual):
                 |$diff
                 |
                 |Update expected output with: scala project/scripts/checkErrorCodeSnippets.scala --with-compiler -- $mdFile --update-output
                 |""".stripMargin
            )
      }

      // Test 2: Check solution snippets compile without warnings
      test(s"$errorCodeStr - solutions must compile") {
        val content = os.read(mdFile)
        val parsed = parseMarkdown(content)

        // Skip validation for error codes from future compiler versions
        assumeWithLogging(
          !isFutureErrorCode(parsed.sinceVersion),
          s"$errorCodeStr introduced in ${parsed.sinceVersion.get.unparse}, current compiler is ${getScriptCompilerVersion().unparse}"
        )

        // Skip validation for inactive error codes
        assumeWithLogging(
          !isInactiveErrorCode(parsed.untilVersion),
          s"$errorCodeStr was made inactive in ${parsed.untilVersion.get.unparse}, current compiler is ${getScriptCompilerVersion().unparse}"
        )

        // Skip validation for non-reproducible error codes
        assumeWithLogging(
          !NonReproducibleErrorCodes.isNonReproducible(errorCode),
          s"$errorCodeStr is non-reproducible - cannot be triggered with pure Scala 3 source code"
        )

        // If all example snippets are sc:nocompile and there are no sc:compile solutions, skip
        assumeWithLogging(
          parsed.solutionSnippets.nonEmpty || parsed.exampleNoCompileSnippets.isEmpty,
          s"$errorCodeStr has only sc:nocompile snippets - requires special circumstances"
        )

        assert(parsed.solutionSnippets.nonEmpty, s"No solution snippets found in $mdFile")

        parsed.solutionSnippets.zipWithIndex.foreach { case (snippet, idx) =>
          val snippetWithWerror = snippet.copy(
            options = snippet.options.filterNot(_.contains("-Werror")) :+ "sc-opts:-Werror"
          )
          val result = compileSnippet(snippetWithWerror)
          assert(
            !result.hasErrors && !result.hasWarnings,
            s"Solution #${idx + 1} should compile without warnings.\nMessages:\n${result.messages.mkString("\n")}"
          )
        }
      }

      if errorCode > Scala3_0_0_MaxErrorCode then
        test(s"$errorCodeStr - must have 'since' version attirubte") {
          os.read(mdFile).linesIterator.find(_.startsWith("since: ")) match {
            case Some(s"since: $version") =>
              assert(ScalaVersion.parse(version).isSuccess, s"Invalid since version: $version")
            case _ => fail(s"No 'from' version attribute found in $mdFile")
          }
        }
    }
  }
