package dotty.tools.dotc.rewrites

import java.nio.file.Files

import scala.io.Codec

import org.junit.Test
import org.junit.Assert.{assertEquals, fail}

import dotty.tools.DottyTest
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.io.{AbstractFile, VirtualFile}

class RewritesTest extends DottyTest:

  /** Applying the patches can fail (here, on overlapping patches); the file must not be clobbered. */
  @Test def failedWriteBackLeavesFileIntact(): Unit =
    val path = Files.createTempFile("RewritesTest", ".scala")
    try
      val content = "class A { def f = 1 }\n"
      Files.writeString(path, content)
      given Context = ctx.fresh.setSetting(ctx.settings.rewrite, true)
      val source = SourceFile(AbstractFile.getFile(path), VirtualFile("_root_", Array.emptyByteArray), Codec.UTF8)
      Rewrites.patch(source, Span(6, 11), "B")
      Rewrites.patch(source, Span(8, 9), "")
      try
        Rewrites.writeBack()
        fail("expected the overlapping patches to be rejected")
      catch case _: AssertionError => ()
      assertEquals(content, Files.readString(path))
    finally Files.delete(path)
