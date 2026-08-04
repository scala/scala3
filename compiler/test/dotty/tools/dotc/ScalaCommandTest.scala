package dotty.tools.dotc

import org.junit.Test
import org.junit.Assert.*
import dotty.tools.dotc.config.Settings.*
import core.Contexts.{Context, ContextBase}
import dotty.tools.io.FileExtension
import dotty.tools.nio.File

import scala.io.Codec

class ScalaCommandTest:

  @Test def `Simple one parameter`: Unit = inContext {
    val settings = config.ScalaSettings
    val args = "-cp path/to/classes1:other/path/to/classes2 files".split(" ")
    val summary = ScalacCommand.distill(args, settings)()
    given SettingsState = summary.sstate
    assertEquals("path/to/classes1:other/path/to/classes2", settings.classpath.value)
    assertEquals("files" :: Nil, summary.arguments)
  }

  @Test def `Unfold @file`: Unit = inContext {
    val settings = config.ScalaSettings
    val file = File.createTemporaryOnDisk("scala-command-test", FileExtension.from("config"))
    file.writeText("-sourceroot myNewRoot someMoreFiles", Codec.UTF8)
    val args = s"-cp path/to/classes1:other/path/to/classes2 @${file.path} someFiles".split(" ")
    val summary = ScalacCommand.distill(args, settings)()
    file.delete()

    given SettingsState = summary.sstate
    assertEquals("path/to/classes1:other/path/to/classes2", settings.classpath.value)
    assertEquals("myNewRoot", settings.sourceroot.value)
    assertEquals("someMoreFiles" :: "someFiles" :: Nil, summary.arguments)
  }

  private def inContext(f: Context ?=> Unit) = f(using (new ContextBase).initialCtx.fresh)

  extension [T](setting: Setting[T])
    private def value(using ss: SettingsState): T = setting.valueIn(ss)
