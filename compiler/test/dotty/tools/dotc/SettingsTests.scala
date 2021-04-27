package dotty.tools
package dotc

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

  @Test def jarOutput: Unit = {
    val source = "tests/pos/Foo.scala"
    val out = Paths.get("out/jaredFoo.jar").normalize
    if (Files.exists(out)) Files.delete(out)
    val options = Array("-classpath", TestConfiguration.basicClasspath, "-d", out.toString, source)
    val reporter = Main.process(options)
    assertEquals(0, reporter.errorCount)
    assertTrue(Files.exists(out))
  }

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

    inContext {
      val args = List("-foo", "b", "-bar", "1")
      val summary = Settings.processArguments(args, true)
      assertTrue(summary.errors.isEmpty)
      given SettingsState = summary.sstate
      assertEquals("b", Settings.foo.value)
      assertEquals(1, Settings.bar.value)
    }

  @Test def validateChoices: Unit =
    object Settings extends SettingGroup:
      val foo = ChoiceSetting("-foo", "foo", "Foo", List("a", "b"), "a")
      val bar = IntChoiceSetting("-bar", "Bar", List(0, 1, 2), 0)
      val baz = IntChoiceSetting("-baz", "Baz", 0 to 10, 10)

      val quux = ChoiceSetting("-quux", "quux", "Quux", List(), "")
      val quuz = IntChoiceSetting("-quuz", "Quuz", List(), 0)

    inContext {
      val args = List("-foo", "b", "-bar", "1", "-baz", "5")
      val summary = Settings.processArguments(args, true)
      assertTrue(summary.errors.isEmpty)
      given SettingsState = summary.sstate
      assertEquals("b", Settings.foo.value)
      assertEquals(1, Settings.bar.value)
      assertEquals(5, Settings.baz.value)
    }

    inContext {
      val args = List("-foo:b")
      val summary = Settings.processArguments(args, true)
      assertTrue(summary.errors.isEmpty)
      given SettingsState = summary.sstate
      assertEquals("b", Settings.foo.value)
    }

    inContext {
      val args = List("-foo", "c", "-bar", "3", "-baz", "-1")
      val summary = Settings.processArguments(args, true)
      val expectedErrors = List(
        "c is not a valid choice for -foo",
        "3 is not a valid choice for -bar",
        "-1 is out of legal range 0..10 for -baz"
      )
      assertEquals(expectedErrors, summary.errors)
    }

    inContext {
      val args = List("-foo:c")
      val summary = Settings.processArguments(args, true)
      val expectedErrors = List("c is not a valid choice for -foo")
      assertEquals(expectedErrors, summary.errors)
    }

    inContext {
      val args = List("-quux", "a", "-quuz", "0")
      val summary = Settings.processArguments(args, true)
      val expectedErrors = List(
        "a is not a valid choice for -quux",
        "0 is not a valid choice for -quuz",
      )
      assertEquals(expectedErrors, summary.errors)
    }

  private def inContext(f: Context ?=> Unit) = f(using (new ContextBase).initialCtx.fresh)

  extension [T](setting: Setting[T])
    private def value(using ss: SettingsState): T = setting.valueIn(ss)
}
