package dotty.tools
package dotc

import scala.language.unsafeNulls

import reporting.StoreReporter
import vulpix.TestConfiguration

import core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.config.Settings._
import dotty.tools.vulpix.TestConfiguration.mkClasspath

import java.nio.file._

import org.junit.Test
import org.junit.Assert._

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
      val foo = StringSetting("-foo", "foo", "Foo", "a")
      val bar = IntSetting("-bar", "Bar", 0)

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
      val option = BooleanSetting("-option", "Some option")

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
      val option = BooleanSetting("-option", "Some option")

    val args = List("-adoption", "dogs", "cats")
    val summary = Settings.processArguments(args, processAll = true)
    assertTrue(summary.errors.isEmpty)
    assertFalse(summary.warnings.isEmpty)
    assertEquals(2, summary.arguments.size)

  @Test def `bad option settings throws`: Unit =
    object Settings extends SettingGroup:
      val option = BooleanSetting("-option", "Some option")

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
      val foo = ChoiceSetting("-foo", "foo", "Foo", List("a", "b"), "a")
      val bar = IntChoiceSetting("-bar", "Bar", List(0, 1, 2), 0)
      val baz = IntChoiceSetting("-baz", "Baz", 0 to 10, 10)

      val quux = ChoiceSetting("-quux", "quux", "Quux", List(), "")
      val quuz = IntChoiceSetting("-quuz", "Quuz", List(), 0)

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
      val foo = IntSetting("-foo", "foo", 80)
    import Settings._

    val args = List("-foo:100")
    val summary = processArguments(args, processAll = true)
    assertTrue(s"Setting args errors:\n  ${summary.errors.take(5).mkString("\n  ")}", summary.errors.isEmpty)
    withProcessedArgs(summary) {
      assertEquals(100, foo.value)
    }

  private def withProcessedArgs(summary: ArgsSummary)(f: SettingsState ?=> Unit) = f(using summary.sstate)

  extension [T](setting: Setting[T])
    private def value(using ss: SettingsState): T = setting.valueIn(ss)
}
