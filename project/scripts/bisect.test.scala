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
