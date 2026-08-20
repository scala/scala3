//> using file bisect.scala
//> using dep org.scalameta::munit::1.3.4

import java.io.File

class BisectOptionsTest extends munit.FunSuite:
  import ValidationCommand.*

  def parse(args: String*): ScriptOptions =
    ScriptOptions.fromArgs(args.toSeq)

  def scriptBody(cmd: ValidationCommand, withBloop: Boolean, withCleaning: Boolean): String =
    val f = cmd.validationScript(withBloop, withCleaning)
    scala.io.Source.fromFile(f).mkString

  test("defaults") {
    val o = parse("compile", "foo.scala")
    assertEquals(o.dryRun, false)
    assertEquals(o.bootstrapped, false)
    assertEquals(o.shouldFail, false)
    assertEquals(o.withBloop, false)
    assertEquals(o.withCleaning, false)
    assertEquals(o.withCleaningExplicit, None)
    assertEquals(o.validationCommand, Compile(Seq("foo.scala")))
  }

  test("bare --dry-run") {
    assertEquals(parse("--dry-run", "compile", "foo.scala").dryRun, true)
  }

  test("--dry-run=false") {
    assertEquals(parse("--dry-run=false", "compile", "foo.scala").dryRun, false)
  }

  test("--dry-run=TRUE") {
    assertEquals(parse("--dry-run=TRUE", "compile", "foo.scala").dryRun, true)
  }

  test("bare --bootstrapped") {
    assertEquals(parse("--bootstrapped", "compile", "foo.scala").bootstrapped, true)
  }

  test("--bootstrapped=false") {
    assertEquals(parse("--bootstrapped=false", "compile", "foo.scala").bootstrapped, false)
  }

  test("bare --should-fail") {
    assertEquals(parse("--should-fail", "compile", "foo.scala").shouldFail, true)
  }

  test("--should-fail=false") {
    assertEquals(parse("--should-fail=false", "compile", "foo.scala").shouldFail, false)
  }

  test("bare --with-bloop implies cleaning") {
    val o = parse("--with-bloop", "compile", "foo.scala")
    assertEquals(o.withBloop, true)
    assertEquals(o.withCleaning, true)
    assertEquals(o.withCleaningExplicit, None)
  }

  test("--with-bloop=false") {
    val o = parse("--with-bloop=false", "compile", "foo.scala")
    assertEquals(o.withBloop, false)
    assertEquals(o.withCleaning, false)
  }

  test("bare --with-cleaning") {
    val o = parse("--with-cleaning", "compile", "foo.scala")
    assertEquals(o.withCleaning, true)
    assertEquals(o.withCleaningExplicit, Some(true))
  }

  test("--with-cleaning=false") {
    val o = parse("--with-cleaning=false", "compile", "foo.scala")
    assertEquals(o.withCleaning, false)
    assertEquals(o.withCleaningExplicit, Some(false))
  }

  test("--with-bloop + --with-cleaning=false") {
    val o = parse("--with-bloop", "--with-cleaning=false", "compile", "foo.scala")
    assertEquals(o.withBloop, true)
    assertEquals(o.withCleaning, false)
    assertEquals(o.withCleaningExplicit, Some(false))
  }

  test("--with-bloop=false + --with-cleaning=true") {
    val o = parse("--with-bloop=false", "--with-cleaning=true", "compile", "foo.scala")
    assertEquals(o.withBloop, false)
    assertEquals(o.withCleaning, true)
    assertEquals(o.withCleaningExplicit, Some(true))
  }

  test("combined flags") {
    val o = parse("--dry-run", "--bootstrapped=true", "--should-fail=false", "--with-bloop", "compile", "foo.scala")
    assertEquals(o.dryRun, true)
    assertEquals(o.bootstrapped, true)
    assertEquals(o.shouldFail, false)
    assertEquals(o.withBloop, true)
    assertEquals(o.withCleaning, true)
  }

  test("--releases range") {
    val o = parse("--releases", "3.1.0...3.2.0", "compile", "foo.scala")
    assertEquals(o.releasesRange, ReleasesRange(Some("3.1.0"), Some("3.2.0")))
  }

  test("invalid boolean value throws") {
    intercept[Throwable] {
      parse("--dry-run=maybe", "compile", "foo.scala")
    }
  }

  test("ValidationCommand.fromArgs maps compile") {
    assertEquals(ValidationCommand.fromArgs(Seq("compile", "foo.scala")), Compile(Seq("foo.scala")))
  }

  test("ValidationCommand.fromArgs maps run") {
    assertEquals(ValidationCommand.fromArgs(Seq("run", "foo.scala")), Run(Seq("foo.scala")))
  }

  test("ValidationCommand.fromArgs maps test") {
    assertEquals(ValidationCommand.fromArgs(Seq("test", "foo.scala")), Test(Seq("foo.scala")))
  }

  test("ValidationCommand.fromArgs maps custom script path") {
    assertEquals(
      ValidationCommand.fromArgs(Seq("my-script.sh")),
      CustomValidationScript(new File("my-script.sh"))
    )
  }

  test("default reproduction script uses --server=false and no clean") {
    val body = scriptBody(Compile(Seq("foo.scala")), withBloop = false, withCleaning = false)
    assert(body.contains("--server=false"))
    assert(!body.contains("scala-cli clean"))
  }

  test("withBloop reproduction script omits --server=false") {
    val body = scriptBody(Compile(Seq("foo.scala")), withBloop = true, withCleaning = false)
    assert(!body.contains("--server=false"))
  }

  test("withCleaning reproduction script runs clean before compile") {
    val body = scriptBody(Compile(Seq("foo.scala")), withBloop = false, withCleaning = true)
    assert(body.contains("scala-cli clean foo.scala"))
    val cleanIdx = body.indexOf("scala-cli clean foo.scala")
    val compileIdx = body.indexOf("scala-cli compile")
    assert(cleanIdx >= 0 && compileIdx > cleanIdx)
  }

class CommitBisectScriptTest extends munit.FunSuite:
  import BuildFailureAction.*

  private val validationScriptPath = "/tmp/validate-bisect.sh"

  def script(
      shouldFail: Boolean = false,
      bootstrapped: Boolean = false,
      onBuildFailure: BuildFailureAction = SkipCommit
  ): String =
    CommitBisectScripts.buildAndValidateScript(validationScriptPath, shouldFail, bootstrapped, onBuildFailure)

  test("build failure skips the commit instead of running git bisect skip") {
    val body = script(onBuildFailure = SkipCommit)
    // `git bisect skip` nested in `git bisect run` moves HEAD and makes the run script exit with
    // the status of the skip itself, which git bisect then records as a verdict for the wrong commit
    assert(!body.contains("git bisect skip"), body)
    assert(body.contains("exit 125"), body)
  }

  test("build failure during edge verification aborts instead of skipping") {
    val body = script(onBuildFailure = AbortBisect)
    assert(body.contains("exit 128"), body)
    assert(!body.contains("exit 125"), body)
  }

  test("aborts when the compiler version cannot be captured") {
    for onBuildFailure <- BuildFailureAction.values do
      val body = script(onBuildFailure = onBuildFailure)
      val guardIdx = body.indexOf("""if [ -z "$scalaVersion" ]""")
      assert(guardIdx >= 0, body)
      assert(body.indexOf("exit 128", guardIdx) >= 0, body)
      assert(guardIdx < body.indexOf(validationScriptPath), body)
  }

  test("version capture filters the sbt output") {
    val body = script()
    val grepIdx = body.indexOf("""grep -E '^[0-9]+\.[0-9]+\.[0-9]+'""")
    assert(grepIdx >= 0, body)
    assert(grepIdx < body.indexOf("tail -n1"), body)
  }

  test("logs the commit under test") {
    val body = script()
    assert(body.contains("git rev-parse --short HEAD"), body)
    assert(body.contains("""echo "Testing commit $commit""""), body)
  }

  test("prints the version of the project matching the bootstrapping") {
    assert(script(bootstrapped = false).contains("print scala3-compiler/version"))
    assert(script(bootstrapped = true).contains("print scala3-compiler-bootstrapped/version"))
  }

  test("shouldFail inverts the validation command status") {
    assert(script(shouldFail = true).contains(s"! $validationScriptPath"))
    val body = script(shouldFail = false)
    assert(!body.contains(s"! $validationScriptPath"), body)
    assert(body.contains(s"""$validationScriptPath "$$scalaVersion""""), body)
  }

  test("publish recipe targets the project matching the bootstrapping") {
    val nonbootstrapped = CommitBisectScripts.sbtPublishRecipe(bootstrapped = false)
    assert(nonbootstrapped.contains("scala3/publishLocal"), nonbootstrapped)
    assert(!nonbootstrapped.contains("scala3-bootstrapped"), nonbootstrapped)
    val bootstrapped = CommitBisectScripts.sbtPublishRecipe(bootstrapped = true)
    assert(bootstrapped.contains("scala3-bootstrapped/publishLocal"), bootstrapped)
  }

class BisectReleasesTest extends munit.FunSuite:
  // Maven Central holds older nightlies; Artifactory nightlies has builds since ~2025-08.
  // Both must be queried so ranges spanning the migration remain usable.
  private val mavenCentralNightly = "3.4.1-RC1-bin-20240125-453658b-NIGHTLY"
  private val artifactoryNightly = "3.10.0-RC1-bin-20260729-8526f78-NIGHTLY"

  test("nightly releases include Maven Central and Artifactory nightlies") {
    val versions = Releases.allReleases.map(_.version).toSet
    assert(versions.contains(mavenCentralNightly), s"missing Maven Central nightly: $mavenCentralNightly")
    assert(versions.contains(artifactoryNightly), s"missing Artifactory nightly: $artifactoryNightly")
  }

  test("releases range spanning Maven Central and Artifactory nightlies") {
    val releases = Releases.fromRange(ReleasesRange(Some(mavenCentralNightly), Some(artifactoryNightly)))
    assertEquals(releases.head.version, mavenCentralNightly)
    assertEquals(releases.last.version, artifactoryNightly)
  }
