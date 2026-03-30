package dotty.tools.dotc
package config

import CommandLineParser.tokenize
import Settings._
import dotty.tools.Useables.given
import dotty.tools.dotc.config.ScalaSettingCategories._
import org.junit.Test
import org.junit.Assert._
import core.Decorators.toMessage
import dotty.tools.io.{Path, PlainFile}

import java.net.URI
import java.nio.file.Files
import scala.util.Using

import scala.annotation.nowarn

class ScalaSettingsTests:

  @Test def `A setting with aliases is accepted`: Unit =
    class MySettings extends SettingGroup:
      val classpath: Setting[String] = PathSetting(RootSetting, "classpath", "Specify where to find user class files.", ".", aliases = List("--class-path", "-cp"))
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
      val language: Setting[List[String]] = MultiStringSetting(RootSetting, "language", "feature", "Enable one or more language features.")
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
      val language: Setting[List[String]] = MultiStringSetting(RootSetting, "language", "feature", "Enable one or more language features.")
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

  @Test def `Don't warn if multistring element is supplied multiply`: Unit =
    class SUT extends SettingGroup:
      val language: Setting[List[String]] = MultiStringSetting(RootSetting, "language", "feature", "Enable one or more language features.")
    val sut  = SUT()
    val args = tokenize("-language:dynamics -language:implicitConversions -language:dynamics")
    val sumy = ArgsSummary(sut.defaultState, args, errors = Nil, warnings = Nil)
    val res  = sut.processArguments(sumy, processAll = true, skipped = Nil)
    val set  = sut.language.valueIn(res.sstate)
    assertEquals(3, args.length)
    assertTrue("Mustn't warn", res.warnings.isEmpty)
    assertTrue("No errors!", res.errors.isEmpty)
    assertTrue("Has the feature", set.contains("implicitConversions"))
    assertTrue("Has the feature", set.contains("dynamics"))

  @Test def `WConf setting is parsed`: Unit =
    import reporting.{Action, Diagnostic, NoExplanation}
    val sets = ScalaSettings
    val args = List("-Wconf:cat=deprecation:s,cat=feature:e", "-Wconf:msg=a problem\\.:s")
    val sumy = ArgsSummary(sets.defaultState, args, errors = Nil, warnings = Nil)
    val proc = sets.processArguments(sumy, processAll = true, skipped = Nil)
    val conf = sets.Wconf.valueIn(proc.sstate)
    val sut  = reporting.WConf.fromSettings(conf).getOrElse(???)
    val msg  = "There was a problem!".toMessage
    val depr = new Diagnostic.DeprecationWarning(msg, util.NoSourcePosition, origin="")
    assertEquals(Action.Silent, sut.action(depr))
    val feat = new Diagnostic.FeatureWarning(msg, util.NoSourcePosition)
    assertEquals(Action.Error, sut.action(feat))
    val warn = new Diagnostic.Warning(msg, util.NoSourcePosition)
    assertEquals(Action.Warning, sut.action(warn))
    val nowr = new Diagnostic.Warning("This is a problem.".toMessage, util.NoSourcePosition)
    assertEquals(Action.Silent, sut.action(nowr))

  @nowarn("cat=deprecation")
  @Test def `Deprecated options are correctly mapped to their replacements`: Unit =
    def createTestCase(oldSetting: Setting[?], newSetting: Setting[?], value: String = "") =
      s"${oldSetting.name}$value" -> newSetting

    val settings = ScalaSettings
    List(
      createTestCase(settings.YtermConflict         , settings.XtermConflict, ":package"),
      createTestCase(settings.YnoGenericSig         , settings.XnoGenericSig),
      createTestCase(settings.Ydumpclasses          , settings.Xdumpclasses,":./"),
      createTestCase(settings.YjarCompressionLevel  , settings.XjarCompressionLevel,":0"),
      createTestCase(settings.YkindProjector        , settings.XkindProjector, ":underscores"),
      createTestCase(settings.YdropComments         , settings.XdropComments),
      createTestCase(settings.YcookComments         , settings.XcookComments),
      createTestCase(settings.YreadComments         , settings.XreadComments),
      createTestCase(settings.YnoDecodeStacktraces  , settings.XnoEnrichErrorMessages),
      createTestCase(settings.YnoEnrichErrorMessages, settings.XnoEnrichErrorMessages),
      createTestCase(settings.YdebugMacros          , settings.XdebugMacros),
      // createTestCase(settings.YjavaTasty            , settings.XjavaTasty),
      // createTestCase(settings.YearlyTastyOutput     , settings.XearlyTastyOutput, ":./"),
      // createTestCase(settings.YallowOutlineFromTasty, settings.XallowOutlineFromTasty),
      createTestCase(settings.YcheckInit            , settings.WsafeInit),
      // createTestCase(settings.Xlint                 , settings.Wshadow, ":all"), // this setting is not going to be mapped to replacement. Read more in the commit message
    ).map: (deprecatedArgument, newSetting) =>
      val args = List(deprecatedArgument)
      val argSummary = ArgsSummary(settings.defaultState, args, errors = Nil, warnings = Nil)
      val conf = settings.processArguments(argSummary, processAll = true, skipped = Nil)
      assert(!newSetting.isDefaultIn(conf.sstate), s"Setting $deprecatedArgument was not forwarded to ${newSetting.name}")

  @nowarn("cat=deprecation")
  @Test def `Deprecated options should not be set if old option was incorrect`: Unit =
    def createTestCase(oldSetting: Setting[?], newSetting: Setting[?], value: String = ":illegal") =
      s"${oldSetting.name}:$value" -> newSetting

    val settings = ScalaSettings
    List(
      createTestCase(settings.YtermConflict         , settings.XtermConflict),
      createTestCase(settings.YnoGenericSig         , settings.XnoGenericSig),
      createTestCase(settings.Ydumpclasses          , settings.Xdumpclasses, ""),
      createTestCase(settings.YjarCompressionLevel  , settings.XjarCompressionLevel),
      createTestCase(settings.YkindProjector        , settings.XkindProjector),
      createTestCase(settings.YdropComments         , settings.XdropComments),
      createTestCase(settings.YcookComments         , settings.XcookComments),
      createTestCase(settings.YreadComments         , settings.XreadComments),
      createTestCase(settings.YnoDecodeStacktraces  , settings.XnoEnrichErrorMessages),
      createTestCase(settings.YnoEnrichErrorMessages, settings.XnoEnrichErrorMessages),
      createTestCase(settings.YdebugMacros          , settings.XdebugMacros),
      // createTestCase(settings.YjavaTasty            , settings.XjavaTasty),
      // createTestCase(settings.YearlyTastyOutput     , settings.XearlyTastyOutput),
      // createTestCase(settings.YallowOutlineFromTasty, settings.XallowOutlineFromTasty),
      createTestCase(settings.YcheckInit            , settings.WsafeInit),
      createTestCase(settings.Xlint                 , settings.Wshadow),
    ).map: (deprecatedArgument, newSetting) =>
      val args = List(deprecatedArgument)
      val argSummary = ArgsSummary(settings.defaultState, args, errors = Nil, warnings = Nil)
      val conf = settings.processArguments(argSummary, processAll = true, skipped = Nil)
      assert(newSetting.isDefaultIn(conf.sstate), s"Setting $deprecatedArgument was forwarded to ${newSetting.name}, when it should be ignored because first option was erroreus")

  // -Xlint was handled in a special way when it was added, making in hard to deprecate it.
  // For now on we will retain old behavior, in next version we will emit deprecation warning.
  // It is also scheduled for removal in future versions.
  @Test def `Make Xlint to ignore invalid args`: Unit =
    val settings = ScalaSettings
    val args = List("-Xlint:-unused,_")
    val argSummary = ArgsSummary(settings.defaultState, args, errors = Nil, warnings = Nil)
    val conf = settings.processArguments(argSummary, processAll = true, skipped = Nil)
    assert(conf.warnings.contains("Option -Xlint is deprecated: Use -Wshadow to enable shadowing lints. Scheduled for removal."))
    assert(conf.errors.isEmpty)

  @nowarn("cat=deprecation")
  @Test def `Aliases of deprecated options are correctly mapped to their replacements`: Unit =
    val settings = ScalaSettings
    val tests = List(
      settings.YtermConflict          -> settings.XtermConflict :* "package",
      settings.YnoGenericSig          -> settings.XnoGenericSig,
      settings.Ydumpclasses           -> settings.Xdumpclasses :* "./",
      settings.YjarCompressionLevel   -> settings.XjarCompressionLevel :* "0",
      settings.YkindProjector         -> settings.XkindProjector :* "underscores",
      settings.YdropComments          -> settings.XdropComments,
      settings.YcookComments          -> settings.XcookComments,
      settings.YreadComments          -> settings.XreadComments,
      settings.YnoDecodeStacktraces   -> settings.XnoEnrichErrorMessages,
      settings.YnoEnrichErrorMessages -> settings.XnoEnrichErrorMessages,
      settings.YdebugMacros           -> settings.XdebugMacros,
      settings.YcheckInit             -> settings.WsafeInit,
      // createTestCase(settings.YjavaTasty            , settings.XjavaTasty),
      // createTestCase(settings.YearlyTastyOutput     , settings.XearlyTastyOutput, ":./"),
      // createTestCase(settings.YallowOutlineFromTasty, settings.XallowOutlineFromTasty),
      // createTestCase(settings.Xlint                 , settings.Wshadow, ":all"), // this setting is not going to be mapped to replacement. Read more in the commit message
    )
    for
      test <- tests
      (old: Setting[?], fwd: Setting[?]) = test.take(2): @unchecked
      SettingAlias(alias, _) <- old.aliases
    do
      assert(old.deprecation.isDefined)
      val argString = if test.size > 2 then s"$alias:${test(2)}" else alias
      val argSummary = ArgsSummary(settings.defaultState, arguments = argString :: Nil, errors = Nil, warnings = Nil)
      val conf = settings.processArguments(argSummary, processAll = true, skipped = Nil)
      def problem = s"Setting alias $argString was not forwarded to ${fwd.name}"
      assert(!fwd.isDefaultIn(conf.sstate), problem)

  @Test def `i18367 rightmost WConf flags take precedence over flags to the left`: Unit =
    import reporting.{Action, Diagnostic}
    val sets = ScalaSettings
    val args = List("-Wconf:cat=deprecation:e", "-Wconf:cat=deprecation:s")
    val sumy = ArgsSummary(sets.defaultState, args, errors = Nil, warnings = Nil)
    val proc = sets.processArguments(sumy, processAll = true, skipped = Nil)
    val conf = sets.Wconf.valueIn(proc.sstate)
    val msg  = "Don't use that!".toMessage
    val depr = new Diagnostic.DeprecationWarning(msg, util.NoSourcePosition, origin="")
    val sut  = reporting.WConf.fromSettings(conf).getOrElse(???)
    assertEquals(Action.Silent, sut.action(depr))


  private def wconfSrcFilterTest(argsStr: String,
                                 warning: reporting.Diagnostic.Warning): Either[List[String], reporting.Action] =
    import reporting.Diagnostic
    val settings = ScalaSettings
    val args = ArgsSummary(settings.defaultState, List(argsStr), errors = Nil, warnings = Nil)
    val proc = settings.processArguments(args, processAll = true, skipped = Nil)
    val wconfStr = settings.Wconf.valueIn(proc.sstate)
    val wconf = reporting.WConf.fromSettings(wconfStr)
    wconf.map(_.action(warning))

  @Test def `WConf src filter silences warnings from a matching path for virtual file`: Unit =
    val result = wconfSrcFilterTest(
      argsStr = "-Wconf:src=path/.*:s",
      warning = reporting.Diagnostic.Warning(
        "A warning".toMessage,
        util.SourcePosition(
          source = util.SourceFile.virtual(new URI("file:///some/path/file.scala"), ""),
          span = util.Spans.Span(1L)
        )
      )
    )
    assertEquals(result, Right(reporting.Action.Silent))

  @Test def `WConf src filter doesn't silence warnings from a non-matching path`: Unit =
    val result = wconfSrcFilterTest(
      argsStr = "-Wconf:src=another/.*:s",
      warning = reporting.Diagnostic.Warning(
        "A warning".toMessage,
        util.SourcePosition(
          source = util.SourceFile.virtual(new URI("file:///some/path/file.scala"), ""),
          span = util.Spans.Span(1L)
        )
      )
    )
    assertEquals(result, Right(reporting.Action.Warning))

  @Test def `WConf src filter silences warnings from a matching path for real file`: Unit =
    val result = Using.resource(Files.createTempFile("myfile", ".scala").nn) { file =>
      wconfSrcFilterTest(
        argsStr = "-Wconf:src=myfile.*?\\.scala:s",
        warning = reporting.Diagnostic.Warning(
          "A warning".toMessage,
          util.SourcePosition(
            source = util.SourceFile(new PlainFile(Path(file)), "UTF-8"),
            span = util.Spans.Span(1L)
          )
        )
      )
    }(using Files.deleteIfExists(_))
    assertEquals(result, Right(reporting.Action.Silent))

  @Test def `WConf src filter doesn't silence warnings from a non-matching path for real file`: Unit =
    val result = Using.resource(Files.createTempFile("myfile", ".scala").nn) { file =>
      wconfSrcFilterTest(
        argsStr = "-Wconf:src=another.*?\\.scala:s",
        warning = reporting.Diagnostic.Warning(
          "A warning".toMessage,
          util.SourcePosition(
            source = util.SourceFile(new PlainFile(Path(file)), "UTF-8"),
            span = util.Spans.Span(1L)
          )
        )
      )
    }(using Files.deleteIfExists(_))
    assertEquals(result, Right(reporting.Action.Warning))

  @Test def `WConf src filter reports an error on an invalid regex`: Unit =
    val result = wconfSrcFilterTest(
      argsStr = """-Wconf:src=\:s""",
      warning = reporting.Diagnostic.Warning(
        "A warning".toMessage,
        util.SourcePosition(
          source = util.SourceFile.virtual(new URI("file:///some/path/file.scala"), ""),
          span = util.Spans.Span(1L)
        )
      ),
    )
    assertTrue(
      result.left.exists(errors =>
        errors.sizeIs == 1 && errors.headOption.exists(_.startsWith("invalid pattern"))
      )
    )

  @Test def `WConf src filter can be mixed with other filters with rightmost taking precedence`: Unit =
    val result = wconfSrcFilterTest(
      argsStr = "-Wconf:src=.*:s,cat=deprecation:e",
      warning = reporting.Diagnostic.DeprecationWarning(
        "A warning".toMessage,
        util.SourcePosition(
          source = util.SourceFile.virtual(new URI("file:///some/path/file.scala"), ""),
          span = util.Spans.Span(1L)
        ),
        origin="",
      )
    )
    assertEquals(result, Right(reporting.Action.Error))

  @Test def `illegal source versions are not accepted when parsing the settings`: Unit =
    for source <- SourceVersion.illegalInSettings do
      val settings = ScalaSettings
      val result = settings.processArguments(List("-source", source.toString()), true)
      assertEquals(0, result.warnings.length)
      assertEquals(1, result.errors.length)

  @Test def `deprecated aliases warn`: Unit =
    val settings = ScalaSettings
    val args = "-Xfatal-warnings" :: "-Xprint" :: "typer" :: Nil
    val result = settings.processArguments(args, processAll = true)
    assertEquals(2, result.warnings.length)
    assertEquals("Option -Xfatal-warnings is a deprecated alias: use -Werror instead", result.warnings.head)
    assertEquals(0, result.errors.length)

  // see sbt-test/pipelining/pipelining-test/test
  @Test def `-Xearly-tasty-output preferPrevious does not warn on change of value`: Unit =
    val settings = ScalaSettings
    val conf = Using.resource(Files.createTempDirectory("testDir")): dir =>
      val args = List("-Ypickle-write", s"$dir/foo.jar", "-Ypickle-write", s"$dir/bar.jar")
      val argSummary = ArgsSummary(settings.defaultState, args, errors = Nil, warnings = Nil)
      settings.processArguments(argSummary, processAll = true, skipped = Nil)
    assert(conf.warnings.isEmpty, s"WARN: ${conf.warnings}")
    assert(conf.errors.isEmpty, s"ERROR: ${conf.errors}")

  @Test def `-Xjava-tasty preferPrevious warns on change of value`: Unit =
    val settings = ScalaSettings
    val args = List("-Xjava-tasty", "-Xjava-tasty:false")
    val argSummary = ArgsSummary(settings.defaultState, args, errors = Nil, warnings = Nil)
    val conf = settings.processArguments(argSummary, processAll = true, skipped = Nil)
    assertEquals(s"Warnings [${conf.warnings}]", 1, conf.warnings.size)
    assertEquals("Ignoring conflicting value for Boolean flag -Xjava-tasty", conf.warnings.head)
    assert(conf.errors.isEmpty, s"ERROR: ${conf.errors}")

end ScalaSettingsTests
