package dotty.tools
package dotc

import reporting.StoreReporter
import vulpix.TestConfiguration

import core.Contexts.{Context, ContextBase}
import dotty.tools.Useables.given
import dotty.tools.dotc.config.Settings.*
import dotty.tools.dotc.config.Settings.Setting.ChoiceWithHelp
import dotty.tools.dotc.config.ScalaSettingCategories.*
import dotty.tools.dotc.config.ScalaSettings
import dotty.tools.vulpix.TestConfiguration.mkClasspath
import dotty.tools.io.PlainDirectory
import dotty.tools.io.Directory
import dotty.tools.dotc.config.ScalaVersion
import io.PlainFile, PlainFile.*

import java.nio.file.*, Files.*

import org.junit.Test
import org.junit.Assert.{assertEquals, assertFalse, assertNotEquals, assertTrue}

import scala.util.Using

class SettingsTests:

  @Test def missingOutputDir: Unit =
    val args = List("-d", "not_here")
    val summary = ScalaSettings.processArguments(args, processAll = true)
    assertEquals(1, summary.errors.size)
    assertEquals("'not_here' does not exist or is not a directory or .jar file", summary.errors.head)

  // if -d rejects its arg, ignore it instead of keeping it as a residual arg
  @Test def `skip failed args`: Unit =
    val args = List("-d", "not_here", "-Vprint")
    val summary = ScalaSettings.processArguments(args, processAll = true)
    assertEquals(2, summary.errors.size)
    assertTrue(summary.arguments.isEmpty)
    assertEquals("'not_here' does not exist or is not a directory or .jar file", summary.errors.head)
    assertEquals("missing argument for option -Vprint", summary.errors.last)

  // same as previous but for colon arg
  @Test def `skip failed colon args`: Unit =
    val args = List("-d:not_here", "-Vprint")
    val summary = ScalaSettings.processArguments(args, processAll = true)
    assertEquals(2, summary.errors.size)
    assertTrue(summary.arguments.isEmpty)
    assertEquals("'not_here' does not exist or is not a directory or .jar file", summary.errors.head)
    assertEquals("missing argument for option -Vprint", summary.errors.last)

  @Test def `output jar file is created eagerly`: Unit =
    import TestConfiguration.basicClasspath
    val source = "tests/pos/Foo.scala"
    val out = Paths.get("out/jarredFoo.jar").normalize
    if Files.exists(out) then Files.delete(out)
    val args = List("-Xmain-class", "Jarred", "-classpath", basicClasspath, "-d", out.toString, source)
    val summary = ScalaSettings.processArguments(args, processAll = true)
    assertEquals(0, summary.errors.size)
    assertTrue(Files.exists(out))

  @Test def `t8124 Don't crash on missing argument`: Unit =
    Using.resource(Files.createTempDirectory("testDir")): dir =>
      val source = Paths.get("tests/pos/Foo.scala").normalize
      val args  = List("-encoding", "-d", dir.toString, source.toString) // -encoding takes an arg!
      val summary = ScalaSettings.processArguments(args, processAll = true)
      assertEquals(1, summary.errors.size)

  @Test def acceptUnconstrained: Unit =
    object Settings extends SettingGroup:
      val foo = StringSetting(RootSetting, "foo", "foo", "Foo", "a")
      val bar = IntSetting(RootSetting, "bar", "Bar", 0)

    val args = List("-foo", "b", "-bar", "1")
    val summary = Settings.processArguments(args, processAll = true)
    assertTrue(summary.errors.isEmpty)
    withProcessedArgs(summary) {
      assertEquals("b", Settings.foo.value)
      assertEquals(1, Settings.bar.value)
    }

  @Test def `workaround dont crash on many files`: Unit =
    object Settings extends SettingGroup

    val args = "--" :: List.fill(6000)("file.scala")
    val summary = Settings.processArguments(args, processAll = true)
    assertTrue(summary.errors.isEmpty)
    assertEquals(6000, summary.arguments.size)

  @Test def `dont crash on many files`: Unit =
    object Settings extends SettingGroup

    val args = List.fill(6000)("file.scala")
    val summary = Settings.processArguments(args, processAll = true)
    assertTrue(summary.errors.isEmpty)
    assertEquals(6000, summary.arguments.size)

  @Test def `dont crash on many options`: Unit =
    object Settings extends SettingGroup:
      val option = StringSetting(RootSetting, "option", "opt", "Some option", "zero")

    val limit = 6000
    val args = List.tabulate(limit)(i => if i % 2 == 0 then "-option" else i.toString)
    val summary = Settings.processArguments(args, processAll = true)
    assertTrue(summary.errors.isEmpty)
    assertEquals(limit/2 - 1, summary.warnings.size)          // should warn on all but first
    assertTrue(summary.warnings.head.contains("was updated"))
    assertEquals(0, summary.arguments.size)
    withProcessedArgs(summary) {
      assertEquals("5999", Settings.option.value.toString)
    }

  @Test def `bad option warning consumes an arg`: Unit =
    object Settings extends SettingGroup:
      val option = BooleanSetting(RootSetting, "option", "Some option")

    val args = List("-adoption", "dogs", "cats")
    val summary = Settings.processArguments(args, processAll = true)
    assertTrue(summary.errors.isEmpty)
    assertFalse(summary.warnings.isEmpty)
    assertEquals(2, summary.arguments.size)
    assertEquals(List("dogs", "cats"), summary.arguments)

  @Test def `bad option settings throws`: Unit =
    object Settings extends SettingGroup:
      val option = BooleanSetting(RootSetting, "option", "Some option")

    def checkMessage(s: String): (Throwable => Boolean) = t =>
      if t.getMessage == s then true
      else
        println(s"Expected: $s, Actual: ${t.getMessage}")
        false

    val default = Settings.defaultState
    assertThrows[IllegalArgumentException](checkMessage("found: not an option of type java.lang.String, required: Boolean")) {
      Settings.option.updateIn(default, "not an option")
    }

  @Test def validateChoices: Unit =
    object Settings extends SettingGroup:
      val foo = ChoiceSetting(RootSetting, "foo", "foo", "Foo", List("a", "b"), "a")
      val bar = IntChoiceSetting(RootSetting, "bar", "Bar", List(0, 1, 2), 0)
      val baz = IntChoiceSetting(RootSetting, "baz", "Baz", 0 to 10, 10)

      val quux = ChoiceSetting(RootSetting, "quux", "quux", "Quux", List(), "")
      val quuz = IntChoiceSetting(RootSetting, "quuz", "Quuz", List(), 0)

    locally {
      val args = List("-foo", "b", "-bar", "1", "-baz", "5")
      val summary = Settings.processArguments(args, true)
      assertTrue(summary.errors.isEmpty)
      withProcessedArgs(summary) {
        assertEquals("b", Settings.foo.value)
        assertEquals(1, Settings.bar.value)
        assertEquals(5, Settings.baz.value)
      }
    }

    locally {
      val args = List("-foo:b")
      val summary = Settings.processArguments(args, true)
      assertTrue(summary.errors.isEmpty)
      withProcessedArgs(summary) {
        assertEquals("b", Settings.foo.value)
      }
    }

    locally {
      val args = List("-foo", "c", "-bar", "3", "-baz", "-1")
      val summary = Settings.processArguments(args, true)
      val expectedErrors = List(
        "c is not a valid choice for -foo",
        "3 is not a valid choice for -bar",
        "-1 is out of legal range 0..10 for -baz"
      )
      assertEquals(expectedErrors, summary.errors)
    }

    locally {
      val args = List("-foo:c")
      val summary = Settings.processArguments(args, true)
      val expectedErrors = List("c is not a valid choice for -foo")
      assertEquals(expectedErrors, summary.errors)
    }

    locally {
      val args = List("-quux", "a", "-quuz", "0")
      val summary = Settings.processArguments(args, true)
      val expectedErrors = List(
        "a is not a valid choice for -quux",
        "0 is not a valid choice for -quuz",
      )
      assertEquals(expectedErrors, summary.errors)
    }
  end validateChoices

  @Test def `Allow IntSetting's to be set with a colon`: Unit =
    object Settings extends SettingGroup:
      val foo = IntSetting(RootSetting, "foo", "foo", 80)
    import Settings._

    def check(args: List[String]) = {
      val summary = processArguments(args, processAll = true)
      assertTrue(s"Setting args errors:\n  ${summary.errors.take(5).mkString("\n  ")}", summary.errors.isEmpty)
      withProcessedArgs(summary) {
        assertEquals(100, foo.value)
      }
    }
    check(List("-foo:100"))
    check(List("-foo", "100"))
    assertThrows[AssertionError](_.getMessage.contains("missing argument for option -foo"))(check(List("-foo")))

  @Test def `option fundamentals`: Unit =
    object Settings extends SettingGroup:
      val option = BooleanSetting(RootSetting, "option", "Some option")
    val args = List("-option", "-option")
    val summary = Settings.processArguments(args, processAll = true)
    assertTrue("Multiple options is not an error", summary.errors.isEmpty)
    assertTrue("Multiple options is not a warning if consistent", summary.warnings.isEmpty)

  @Test def `boolean option fundamentals`: Unit =
    object Settings extends SettingGroup:
      val option = BooleanSetting(RootSetting, "option", "Some option")
    val args = List("-option", "-option:false")
    val summary = Settings.processArguments(args, processAll = true)
    assertTrue("Multiple options is not an error", summary.errors.isEmpty)
    assertFalse("Multiple conflicting options is a warning", summary.warnings.isEmpty)
    assertTrue(summary.warnings.forall(_.contains("Conflicting")))

  @Test def `string option may be consistent`: Unit =
    object Settings extends SettingGroup:
      val option = StringSetting(RootSetting, "option", "opt", "Some option", "none")
    val args = List("-option:something", "-option:something")
    val summary = Settings.processArguments(args, processAll = true)
    assertTrue("Multiple options is not an error", summary.errors.isEmpty)
    assertTrue("Multiple consistent options is not a warning", summary.warnings.isEmpty)

  @Test def `string option must be consistent`: Unit =
    object Settings extends SettingGroup:
      val option = StringSetting(RootSetting, "option", "opt", "Some option", "none")
    val args = List("-option:something", "-option:nothing")
    val summary = Settings.processArguments(args, processAll = true)
    assertTrue("Multiple options is not an error", summary.errors.isEmpty)
    assertFalse("Multiple conflicting options is a warning", summary.warnings.isEmpty)
    assertTrue(summary.warnings.forall(_.contains("updated")))

  @Test def `int option also warns`: Unit =
    object Settings extends SettingGroup:
      val option = IntSetting(RootSetting, "option", "Some option", 42)
    val args = List("-option:17", "-option:27")
    val summary = Settings.processArguments(args, processAll = true)
    assertTrue("Multiple options is not an error", summary.errors.isEmpty)
    assertFalse("Multiple conflicting options is a warning", summary.warnings.isEmpty)
    assertTrue(summary.warnings.forall(_.contains("updated")))

  @Test def `dir option also warns`: Unit =
    val abc: PlainFile = Paths.get("a", "b", "c").toPlainFile
    object Settings extends SettingGroup:
      val option = OutputSetting(RootSetting, "option", "out", "A file", Paths.get("a", "b", "c").toPlainFile)
    Using.resource(createTempDirectory("i13887")) { dir =>
      val target = createDirectory(dir.resolve("x"))
      val mistake = createDirectory(dir.resolve("y"))
      val args = List("-option", target.toString, "-option", mistake.toString)
      val summary = Settings.processArguments(args, processAll = true)
      assertTrue("Multiple options is not an error", summary.errors.isEmpty)
      assertFalse("Multiple conflicting options is a warning", summary.warnings.isEmpty)
      assertTrue(summary.warnings.forall(_.contains("updated")))
    }

  @Test def `Set BooleanSettings correctly`: Unit =
    object Settings extends SettingGroup:
      val foo = BooleanSetting(RootSetting, "foo", "foo", initialValue = false)
      val bar = BooleanSetting(RootSetting, "bar", "bar", initialValue = true)
      val baz = BooleanSetting(RootSetting, "baz", "baz", initialValue = false)
      val qux = BooleanSetting(RootSetting, "qux", "qux", initialValue = false)
    import Settings._

    val args = List("-foo:true", "-bar:false", "-baz", "-qux:true", "-qux:false")
    val summary = processArguments(args, processAll = true)
    assertTrue(s"Setting args errors:\n  ${summary.errors.take(5).mkString("\n  ")}", summary.errors.isEmpty)
    withProcessedArgs(summary):
      assertEquals(true, foo.value)
      assertEquals(false, bar.value)
      assertEquals(true, baz.value)
      assertEquals(false, qux.value)
      assertEquals(List("Conflicting value for Boolean flag -qux"), summary.warnings)

  @Test def `flag can't be set with separate arg`: Unit =
    object Settings extends SettingGroup:
      val foo = BooleanSetting(RootSetting, "foo", "foo", initialValue = false)
    import Settings._

    val args = List("-foo", "false")
    val summary = processArguments(args, processAll = true)
    withProcessedArgs(summary):
      assertTrue("Nothing to see here", summary.errors.isEmpty)
      assertTrue("Nothing to see here", summary.warnings.isEmpty)
      assertEquals(true, foo.value)

  @Test def `Output setting is overriding existing jar`: Unit =
    val result = Using.resource(Files.createTempFile("myfile", ".jar")): file =>
      object Settings extends SettingGroup:
        val defaultDir = new PlainDirectory(Directory("."))
        val testOutput = OutputSetting(RootSetting, "testOutput", "testOutput", "", defaultDir)

      import Settings.*
      Files.write(file, "test".getBytes())
      val fileStateBefore = String(Files.readAllBytes(file))
      val args = List(s"-testOutput:${file.toString}")
      val summary = processArguments(args, processAll = true)

      assertNotEquals(fileStateBefore, String(Files.readAllBytes(file)), "Jar should have been overriden")

  @Test def `Output setting respects previous setting`: Unit =
    val result = Using.resources(
      Files.createTempFile("myfile", ".jar"), Files.createTempFile("myfile2", ".jar")
    ): (file1, file2) =>
      object Settings extends SettingGroup:
        val defaultDir = new PlainDirectory(Directory("."))
        val testOutput = OutputSetting(RootSetting, "testOutput", "testOutput", "", defaultDir, preferPrevious = true)

      import Settings._

      Files.write(file1, "test1".getBytes())
      Files.write(file2, "test2".getBytes())

      val file1StateBefore = String(Files.readAllBytes(file1))
      val file2StateBefore = String(Files.readAllBytes(file2))

      val creationTime = Files.getLastModifiedTime(file1)
      val args = List(s"-testOutput:${file1.toString}", s"-testOutput:${file2.toString}")
      val summary = processArguments(args, processAll = true)

      // The output is a new filesystem without information of original path
      // We can't check the `testOutput.value` as in other tests.
      assertNotEquals(file1StateBefore, String(Files.readAllBytes(file1)))
      assertEquals(file2StateBefore, String(Files.readAllBytes(file2)))

  @Test def `Output side effect is not present when setting is deprecated`: Unit =
    val result = Using.resource(Files.createTempFile("myfile", ".jar")): file =>
      object Settings extends SettingGroup:
        val defaultDir = new PlainDirectory(Directory("."))
        val testOutput = OutputSetting(RootSetting, "testOutput", "testOutput", "", defaultDir, preferPrevious = true, deprecation = Deprecation.renamed("XtestOutput"))

      import Settings._

      Files.write(file, "test".getBytes())
      val fileStateBefore = String(Files.readAllBytes(file))

      val args = List(s"-testOutput:${file.toString}")
      val summary = processArguments(args, processAll = true)

      assertEquals(fileStateBefore, String(Files.readAllBytes(file)))

  @Test def `Arguments of options are correctly parsed with either ":" or " " separators`: Unit =
    val Help = "" // i.e. help = Help
    object Settings extends SettingGroup:
      val booleanSetting = BooleanSetting(RootSetting, "booleanSetting", "booleanSetting", false)
      val stringSetting  = StringSetting(RootSetting, "stringSetting", "stringSetting", Help, "test")
      val choiceSetting =  ChoiceSetting(RootSetting, "choiceSetting", "choiceSetting", Help, List("a", "b"), "a")
      val multiChoiceSetting = MultiChoiceSetting(RootSetting, "multiChoiceSetting", "multiChoiceSetting", Help, choices = List("a", "b"), legacyChoices = List("c"))
      val multiChoiceHelpSetting=  MultiChoiceHelpSetting(RootSetting, "multiChoiceHelpSetting", "multiChoiceHelpSetting", Help, List(ChoiceWithHelp("a", "a"), ChoiceWithHelp("b", "b")), List(), legacyChoices = List("c"))
      val intSetting = IntSetting(RootSetting, "intSetting", "intSetting", 0)
      val intChoiceSetting = IntChoiceSetting(RootSetting, "intChoiceSetting", "intChoiceSetting", List(1,2,3), 1)
      val multiStringSetting = MultiStringSetting(RootSetting, "multiStringSetting", "multiStringSetting", Help, default = List("a", "b"))
      val outputSetting = OutputSetting(RootSetting, "outputSetting", "outputSetting", Help, new PlainDirectory(Directory(".")))
      val pathSetting = PathSetting(RootSetting, "pathSetting", "pathSetting", ".")
      val phasesSetting = PhasesSetting(RootSetting, "phasesSetting", "phasesSetting", "all")
      val versionSetting= VersionSetting(RootSetting, "versionSetting", "versionSetting")

    import Settings._
    Using.resource(Files.createTempDirectory("testDir")): dir =>

      val args = List(
        List("-booleanSetting", "true"), // `-b false` does not mean `-b:false`
        List("-stringSetting", "newTest"),
        List("-choiceSetting", "b"),
        List("-multiChoiceSetting", "a,b,c"),
        List("-multiChoiceHelpSetting", "a,b,c"),
        List("-intSetting", "42"),
        List("-intChoiceSetting", "2"),
        List("-multiStringSetting", "a,b"),
        List("-outputSetting", dir.toString),
        List("-pathSetting", dir.toString),
        List("-phasesSetting", "parser,typer"),
        List("-versionSetting", "1.0.0"),
      )

      def testValues(summary: ArgsSummary) =
        withProcessedArgs(summary):
          assertEquals(true, booleanSetting.value)
          assertEquals("newTest", stringSetting.value)
          assertEquals("b", choiceSetting.value)
          assertEquals(List("a", "b"), multiChoiceSetting.value)
          assertEquals(List("a", "b"), multiChoiceHelpSetting.value)
          assertEquals(42, intSetting.value)
          assertEquals(2, intChoiceSetting.value)
          assertEquals(List("a", "b"), multiStringSetting.value)
          assertEquals(dir.toString, outputSetting.value.path)
          assertEquals(dir.toString, pathSetting.value)
          assertEquals(List("parser", "typer"), phasesSetting.value)
          assertEquals(ScalaVersion.parse("1.0.0").get, versionSetting.value)

      val summaryColon = processArguments(args.map(_.mkString(":")), processAll = true)
      val summaryWhitespace = processArguments(args.flatten, processAll = true)
      testValues(summary = summaryColon)
      testValues(summary = summaryWhitespace)

  @Test def `prefix option requires nonempty suffix`: Unit =
    object Settings extends SettingGroup:
      val jvmargs  = PrefixSetting(RootSetting, "J<flag>", "Pass -J<flag> directly to the runtime system.")
    import Settings.*
    val args = List("-J")
    val summary = processArguments(args, processAll = true)
    withProcessedArgs(summary):
      assertTrue("Nothing to see here", summary.errors.isEmpty)
      assertEquals(1, summary.warnings.length)
      assertTrue("Prefix option warns", summary.warnings.head.contains("requires a suffix"))
      assertTrue("Prefix option is empty", jvmargs.value.isEmpty)

  @Test def `alias deprecation is honored`: Unit =
    object Settings extends SettingGroup:
      val flag = BooleanSetting(RootSetting, "yas", "Yes, do it!",
        aliases = SettingAlias("-meh", Deprecation("that was a dumb flag name")) :: Nil,
      )
    import Settings.*
    val args = List("-meh")
    val summary = processArguments(args, processAll = true)
    withProcessedArgs(summary):
      assertTrue("Nothing to see here", summary.errors.isEmpty)
      assertEquals(1, summary.warnings.length)
      assertEquals("Option -meh is a deprecated alias: that was a dumb flag name", summary.warnings.head)
      assertTrue("flag was set anyway", flag.value)

  @Test def `alias deprecation is honored when primary option is deprecated`: Unit =
    object Settings extends SettingGroup:
      val flag = BooleanSetting(RootSetting, "yas", "Yes, do it!",
        aliases = SettingAlias("-meh", Deprecation("that was a dumb flag name")) :: Nil,
        deprecation = Deprecation.removed(),
      )
    import Settings.*
    val args = List("-meh")
    val summary = processArguments(args, processAll = true)
    withProcessedArgs(summary):
      assertTrue("Nothing to see here", summary.errors.isEmpty)
      assertEquals(2, summary.warnings.length)
      assertEquals("Option -meh is a deprecated alias: that was a dumb flag name", summary.warnings.head)
      assertEquals("Option -yas is deprecated: Scheduled for removal.", summary.warnings(1))
      assertTrue("flag was set anyway", flag.value)

  @Test def `alias deprecation respects primary rename`: Unit =
    object Settings extends SettingGroup:
      val flag = BooleanSetting(RootSetting, "yas", "Yes, do it!",
        aliases = SettingAlias("-meh", Deprecation("that was a dumb flag name")) :: Nil,
        deprecation = Deprecation.renamed("anything!"),
      )
    import Settings.*
    val args = List("-meh")
    val summary = processArguments(args, processAll = true)
    withProcessedArgs(summary):
      assertTrue("Nothing to see here", summary.errors.isEmpty)
      assertEquals(2, summary.warnings.length)
      assertEquals("Option -meh is a deprecated alias: that was a dumb flag name", summary.warnings.head)
      assertEquals("Option -yas is deprecated: Use anything! instead.", summary.warnings(1))
      assertFalse("flag was set anyway", flag.value)

  @Test def `alias deprecation can't be replaced by anything`: Unit =
    object Settings extends SettingGroup:
      val flag = BooleanSetting(RootSetting, "yas", "Yes, do it!",
        aliases = SettingAlias("-meh", Deprecation.renamed("anything at all!")) :: Nil,
      )
    import Settings.*
    assertThrows[AssertionError](_.getMessage.contains("replaced by")):
      flag

  @Test def `Ignored setting shifts args`: Unit =
    object Settings extends SettingGroup:
      val foo = BooleanSetting(RootSetting, "foo", "foo", ignoreInvalidArgs = true, preferPrevious = true)
      val bar = BooleanSetting(RootSetting, "bar", "bar")
      val baz = OutputSetting(RootSetting, "out", "dir", "A file", default = Paths.get("out", "baz").toPlainFile,
        ignoreInvalidArgs = true, preferPrevious = true)
    import Settings.*
    Using.resource(createTempDirectory("testDir")): dir =>
      val out = createDirectory(dir.resolve("x"))
      val args = List("-out", out.toString, "-out", s"$dir/y", "-foo:true", "-foo:false", "-bar:true")
      val summary = processArguments(args, processAll = true)
      assertTrue(summary.errors.mkString(","), summary.errors.isEmpty)
      assertEquals(1, summary.warnings.size)
      assertEquals("Ignoring conflicting value for Boolean flag -foo", summary.warnings.head)
      assertEquals(1L, Files.list(dir).count) // second -out is ignored, no dir/y exists
      withProcessedArgs(summary):
        assertTrue(foo.value)
        assertTrue(bar.value)

  // use the supplied summary for evaluating settings
  private def withProcessedArgs(summary: ArgsSummary)(f: SettingsState ?=> Unit) = f(using summary.sstate)

  // evaluate a setting using only a SettingsState (instead of a full-blown Context)
  extension [T](setting: Setting[T])
    private def value(using ss: SettingsState): T = setting.valueIn(ss)
