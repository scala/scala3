package dotty.tools.dotc.rewrites

import scala.io.Codec

import org.junit.Test
import org.junit.Assert.{assertEquals, fail}

import dotty.tools.DottyTest
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.nio.*

class RewritesTest extends DottyTest:

  /** Applying the patches can fail (here, on overlapping patches); the file must not be clobbered. */
  @Test def failedWriteBackLeavesFileIntact(): Unit =
    val path = File.createTemporaryOnDisk("RewritesTest.scala")
    val virtualRoot = FileContainer.createInMemory("_root")
    try
      val content = "class A { def f = 1 }\n"
      path.writeText(content, Codec.UTF8)
      given Context = ctx.fresh.setSetting(ctx.settings.rewrite, true)
      val source = SourceFile(path, virtualRoot, Codec.UTF8)
      Rewrites.patch(source, Span(6, 11), "B")
      Rewrites.patch(source, Span(8, 9), "")
      try
        Rewrites.writeBack()
        fail("expected the overlapping patches to be rejected")
      catch case _: AssertionError => ()
      assertEquals(content, path.readText(Codec.UTF8))
    finally path.delete()
