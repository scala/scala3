package dotty.tools

import vulpix.TestConfiguration

import org.junit.Test

import dotc.ast.Trees._
import dotc.core.Decorators._
import dotc.core.Contexts._
import dotc.core.Types._
import dotc.core.Denotations._

import java.io.File
import java.nio.file._

class SignaturesTest:
  @Test def signatureCaching: Unit =
    inCompilerContext(TestConfiguration.basicClasspath, "case class Foo(value: Unit)") {
      val defn = ctx.definitions
      val leftCls = ctx.requiredClass("Foo")
      val ref = leftCls.requiredMethod("value").termRef

      def checkSig()(using Context): Unit =
        val denot = ref.denot match
          case sd: SingleDenotation => sd.initial
          case d => d
        assert(ref.signature == denot.signature, i"Wrong cached signature at phase ${ctx.phase} for $ref.\nActual denotation signature: ${denot.signature}\nCached ref signature: ${ref.signature}")

      checkSig()
      checkSig()(using ctx.withPhase(ctx.erasurePhase.next))
    }
