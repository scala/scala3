package dotty.tools.dotc
package config

import CommandLineParser.tokenize
import Settings._

import org.junit.Test
import org.junit.Assert._

class ScalaSettingsTests:

  @Test def `A setting with aliases is accepted`: Unit =
    class MySettings extends SettingGroup:
      val classpath: Setting[String] = PathSetting("-classpath", "Specify where to find user class files.", ".", aliases = List("--class-path", "-cp"))
    val settings = MySettings()
    val args = tokenize("-cp path/to/classes1:other/path/to/classes2")
    val summary = ArgsSummary(settings.defaultState, args, errors = Nil, warnings = Nil)
    val res  = settings.processArguments(summary, processAll = true, skipped = Nil)
    val classpath = settings.classpath.valueIn(res.sstate)
    assertEquals(2, args.length)
    assertTrue(s"found warnings: ${res.warnings}", res.warnings.isEmpty)
    assertTrue(s"found errors: ${res.errors}", res.errors.isEmpty)
    assertTrue("wrong classpath", classpath == "path/to/classes1:other/path/to/classes2")

  @Test def `A multistring setting is multivalued`: Unit =
    class SUT extends SettingGroup:
      val language: Setting[List[String]] = MultiStringSetting("-language", "feature", "Enable one or more language features.")
    val sut  = SUT()
    val args = tokenize("-language:implicitConversions,dynamics")
    val sumy = ArgsSummary(sut.defaultState, args, errors = Nil, warnings = Nil)
    val res  = sut.processArguments(sumy, processAll = true, skipped = Nil)
    val set  = sut.language.valueIn(res.sstate)
    assertEquals(1, args.length)
    assertTrue("No warnings!", res.warnings.isEmpty)
    assertTrue("No errors!", res.errors.isEmpty)
    assertTrue("Has the feature", set.contains("implicitConversions"))
    assertTrue("Has the feature", set.contains("dynamics"))

  @Test def `t9719 Apply -language more than once`: Unit =
    class SUT extends SettingGroup:
      val language: Setting[List[String]] = MultiStringSetting("-language", "feature", "Enable one or more language features.")
    val sut  = SUT()
    val args = tokenize("-language:implicitConversions -language:dynamics")
    val sumy = ArgsSummary(sut.defaultState, args, errors = Nil, warnings = Nil)
    val res  = sut.processArguments(sumy, processAll = true, skipped = Nil)
    val set  = sut.language.valueIn(res.sstate)
    assertEquals(2, args.length)
    assertTrue("No warnings!", res.warnings.isEmpty)
    assertTrue("No errors!", res.errors.isEmpty)
    assertTrue("Has the feature", set.contains("implicitConversions"))
    assertTrue("Has the feature", set.contains("dynamics"))

  @Test def `Warn if multistring element is supplied multiply`: Unit =
    class SUT extends SettingGroup:
      val language: Setting[List[String]] = MultiStringSetting("-language", "feature", "Enable one or more language features.")
    val sut  = SUT()
    val args = tokenize("-language:dynamics -language:implicitConversions -language:dynamics")
    val sumy = ArgsSummary(sut.defaultState, args, errors = Nil, warnings = Nil)
    val res  = sut.processArguments(sumy, processAll = true, skipped = Nil)
    val set  = sut.language.valueIn(res.sstate)
    assertEquals(3, args.length)
    assertEquals("Must warn", 1, res.warnings.length)
    assertTrue("No errors!", res.errors.isEmpty)
    assertTrue("Has the feature", set.contains("implicitConversions"))
    assertTrue("Has the feature", set.contains("dynamics"))

  @Test def `WConf setting is parsed`: Unit =
    import reporting.{Action, Diagnostic, NoExplanation}
    val sets = new ScalaSettings
    val args = List("-Wconf:cat=deprecation:s,cat=feature:e", "-Wconf:msg=a problem\\.:s")
    val sumy = ArgsSummary(sets.defaultState, args, errors = Nil, warnings = Nil)
    val proc = sets.processArguments(sumy, processAll = true, skipped = Nil)
    val conf = sets.Wconf.valueIn(proc.sstate)
    val sut  = reporting.WConf.fromSettings(conf).getOrElse(???)
    val msg  = NoExplanation("There was a problem!")
    val depr = new Diagnostic.DeprecationWarning(msg, util.NoSourcePosition)
    assertEquals(Action.Silent, sut.action(depr))
    val feat = new Diagnostic.FeatureWarning(msg, util.NoSourcePosition)
    assertEquals(Action.Error, sut.action(feat))
    val warn = new Diagnostic.Warning(msg, util.NoSourcePosition)
    assertEquals(Action.Warning, sut.action(warn))
    val nowr = new Diagnostic.Warning(NoExplanation("This is a problem."), util.NoSourcePosition)
    assertEquals(Action.Silent, sut.action(nowr))

end ScalaSettingsTests
