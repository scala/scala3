package dotty.tools
package dotc

import scala.language.unsafeNulls

import reporting.StoreReporter
import vulpix.TestConfiguration

import core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.config.Settings.*
import dotty.tools.dotc.config.Settings.Setting.ChoiceWithHelp
import dotty.tools.dotc.config.ScalaSettingCategories.*
import dotty.tools.vulpix.TestConfiguration.mkClasspath
import dotty.tools.io.PlainDirectory
import dotty.tools.io.Directory
import dotty.tools.dotc.config.ScalaVersion

import java.nio.file._

import org.junit.Test
import org.junit.Assert._
import scala.util.Using

class SettingsTests {

  @Test def missingOutputDir: Unit =
    val options = Array("-d", "not_here")
    val reporter = Main.process(options, reporter = StoreReporter())
    assertEquals(1, reporter.errorCount)
    assertEquals("'not_here' does not exist or is not a directory or .jar file", reporter.allErrors.head.message)

  @Test def jarOutput: Unit =
    val source = "tests/pos/Foo.scala"
    val out = Paths.get("out/jaredFoo.jar").normalize
    if (Files.exists(out)) Files.delete(out)
    val options = Array("-classpath", TestConfiguration.basicClasspath, "-d", out.toString, source)
    val reporter = Main.process(options)
    assertEquals(0, reporter.errorCount)
    assertTrue(Files.exists(out))

  @Test def `t8124 Don't crash on missing argument`: Unit =
    val source    = Paths.get("tests/pos/Foo.scala").normalize
    val outputDir = Paths.get("out/testSettings").normalize
    if Files.notExists(outputDir) then Files.createDirectory(outputDir)
    // -encoding takes an arg!
    val options  = Array("-encoding", "-d", outputDir.toString, source.toString)
    val reporter = Main.process(options, reporter = StoreReporter())
    assertEquals(1, reporter.errorCount)

  @Test def acceptUnconstrained: Unit =
    object Settings extends SettingGroup:
      val foo = StringSetting(RootSetting, "foo", "foo", "Foo", "a")
      val bar = IntSetting(RootSetting, "bar", "Bar", 0)

    val args = List("-foo", "b", "-bar", "1")
    val summary = Settings.processArguments(args, true)
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
      val option = BooleanSetting(RootSetting, "option", "Some option")

    val limit = 6000
    val args = List.fill(limit)("-option")
    val summary = Settings.processArguments(args, processAll = true)
    assertTrue(summary.errors.isEmpty)
    assertEquals(limit-1, summary.warnings.size)
    assertTrue(summary.warnings.head.contains("repeatedly"))
    assertEquals(0, summary.arguments.size)
    withProcessedArgs(summary) {
      assertTrue(Settings.option.value)
    }

  @Test def `bad option warning consumes an arg`: Unit =
    object Settings extends SettingGroup:
      val option = BooleanSetting(RootSetting, "option", "Some option")

    val args = List("-adoption", "dogs", "cats")
    val summary = Settings.processArguments(args, processAll = true)
    assertTrue(summary.errors.isEmpty)
    assertFalse(summary.warnings.isEmpty)
    assertEquals(2, summary.arguments.size)

  @Test def `bad option settings throws`: Unit =
    object Settings extends SettingGroup:
      val option = BooleanSetting(RootSetting, "option", "Some option")

    def checkMessage(s: String): (Throwable => Boolean) = t =>
      if t.getMessage == s then true
      else
        println(s"Expected: $s, Actual: ${t.getMessage}")
        false

    val default = Settings.defaultState
    dotty.tools.assertThrows[IllegalArgumentException](checkMessage("found: not an option of type java.lang.String, required: Boolean")) {
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

  @Test def `Allow IntSetting's to be set with a colon`: Unit =
    object Settings extends SettingGroup:
      val foo = IntSetting(RootSetting, "foo", "foo", 80)
    import Settings._

    val args = List("-foo:100")
    val summary = processArguments(args, processAll = true)
    assertTrue(s"Setting args errors:\n  ${summary.errors.take(5).mkString("\n  ")}", summary.errors.isEmpty)
    withProcessedArgs(summary) {
      assertEquals(100, foo.value)
    }

  @Test def `Set BooleanSettings correctly`: Unit =
    object Settings extends SettingGroup:
      val foo = BooleanSetting(RootSetting, "foo", "foo", false)
      val bar = BooleanSetting(RootSetting, "bar", "bar", true)
      val baz = BooleanSetting(RootSetting, "baz", "baz", false)
      val qux = BooleanSetting(RootSetting, "qux", "qux", false)
    import Settings._

    val args = List("-foo:true", "-bar:false", "-baz", "-qux:true", "-qux:false")
    val summary = processArguments(args, processAll = true)
    assertTrue(s"Setting args errors:\n  ${summary.errors.take(5).mkString("\n  ")}", summary.errors.isEmpty)
    withProcessedArgs(summary) {
      assertEquals(true, foo.value)
      assertEquals(false, bar.value)
      assertEquals(true, baz.value)
      assertEquals(false, qux.value)
      assertEquals(List("Flag -qux set repeatedly"), summary.warnings)
    }

  @Test def `Output setting is overriding existing jar`: Unit =
    val result = Using.resource(Files.createTempFile("myfile", ".jar").nn){ file =>
      object Settings extends SettingGroup:
        val defaultDir = new PlainDirectory(Directory("."))
        val testOutput = OutputSetting(RootSetting, "testOutput", "testOutput", "", defaultDir)

      import Settings._

      Files.write(file, "test".getBytes())
      val fileStateBefore = String(Files.readAllBytes(file))

      val args = List(s"-testOutput:${file.toString}")
      val summary = processArguments(args, processAll = true)

      assertNotEquals(fileStateBefore, String(Files.readAllBytes(file)), "Jar should have been overriden")

    }(Files.deleteIfExists(_))

  @Test def `Output setting is respecting previous setting`: Unit =
    val result = Using.resources(
      Files.createTempFile("myfile", ".jar").nn, Files.createTempFile("myfile2", ".jar").nn
    ){ (file1, file2) =>
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

    }(Files.deleteIfExists(_), Files.deleteIfExists(_))

  @Test def `Output side effect is not present when setting is deprecated`: Unit =
    val result = Using.resource(Files.createTempFile("myfile", ".jar").nn){ file =>
      object Settings extends SettingGroup:
        val defaultDir = new PlainDirectory(Directory("."))
        val testOutput = OutputSetting(RootSetting, "testOutput", "testOutput", "", defaultDir, preferPrevious = true, deprecation = Deprecation.renamed("XtestOutput"))

      import Settings._

      Files.write(file, "test".getBytes())
      val fileStateBefore = String(Files.readAllBytes(file))

      val args = List(s"-testOutput:${file.toString}")
      val summary = processArguments(args, processAll = true)

      assertEquals(fileStateBefore, String(Files.readAllBytes(file)))

    }(Files.deleteIfExists(_))

  @Test def `Arguments of flags are correctly parsed with both ":" and " " separating`: Unit =
    object Settings extends SettingGroup:
      val booleanSetting = BooleanSetting(RootSetting, "booleanSetting", "booleanSetting", false)
      val stringSetting  = StringSetting(RootSetting, "stringSetting", "stringSetting", "", "test")
      val choiceSetting =  ChoiceSetting(RootSetting, "choiceSetting", "choiceSetting", "", List("a", "b"), "a")
      val multiChoiceSetting=  MultiChoiceSetting(RootSetting, "multiChoiceSetting", "multiChoiceSetting", "", List("a", "b"), List(), legacyChoices = List("c"))
      val multiChoiceHelpSetting=  MultiChoiceHelpSetting(RootSetting, "multiChoiceHelpSetting", "multiChoiceHelpSetting", "", List(ChoiceWithHelp("a", "a"), ChoiceWithHelp("b", "b")), List(), legacyChoices = List("c"))
      val intSetting = IntSetting(RootSetting, "intSetting", "intSetting", 0)
      val intChoiceSetting = IntChoiceSetting(RootSetting, "intChoiceSetting", "intChoiceSetting", List(1,2,3), 1)
      val multiStringSetting = MultiStringSetting(RootSetting, "multiStringSetting", "multiStringSetting", "", List("a", "b"), List())
      val outputSetting = OutputSetting(RootSetting, "outputSetting", "outputSetting", "", new PlainDirectory(Directory(".")))
      val pathSetting = PathSetting(RootSetting, "pathSetting", "pathSetting", ".")
      val phasesSetting = PhasesSetting(RootSetting, "phasesSetting", "phasesSetting", "all")
      val versionSetting= VersionSetting(RootSetting, "versionSetting", "versionSetting")

    import Settings._
    Using.resource(Files.createTempDirectory("testDir")) { dir =>

      val args = List(
        List("-booleanSetting", "true"),
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
        withProcessedArgs(summary) {
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
        }

      val summaryColon = processArguments(args.map(_.mkString(":")), processAll = true)
      val summaryWhitespace = processArguments(args.flatten, processAll = true)
      testValues(summary = summaryColon)
      testValues(summary = summaryWhitespace)

    }(Files.deleteIfExists(_))

  private def withProcessedArgs(summary: ArgsSummary)(f: SettingsState ?=> Unit) = f(using summary.sstate)

  extension [T](setting: Setting[T])
    private def value(using ss: SettingsState): T = setting.valueIn(ss)
}
