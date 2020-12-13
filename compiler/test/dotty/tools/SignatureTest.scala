package dotty.tools

import vulpix.TestConfiguration

import org.junit.Test

import dotc.ast.Trees._
import dotc.core.Decorators._
import dotc.core.Contexts._
import dotc.core.Phases._
import dotc.core.Types._
import dotc.core.Symbols._

import java.io.File
import java.nio.file._

class SignatureTest:
  @Test def signatureCaching: Unit =
    inCompilerContext(TestConfiguration.basicClasspath, separateRun = true, "case class Foo(value: Unit)") {
      val (ref, refSig) = atPhase(erasurePhase.next) {
        val cls = requiredClass("Foo")
        val ref = cls.requiredMethod("value").termRef
        (ref, ref.signature)
      }
      atPhase(typerPhase) {
        // NamedType#signature is always computed before erasure, which ensures
        // that it stays stable and therefore can be cached as long as
        // signatures are guaranteed to be stable before erasure, see the
        // comment above `Compiler#phases`.
        assert(refSig == ref.signature,
          s"""The signature of a type should never change but the signature of $ref was:
             |${ref.signature} at typer, whereas it was:
             |${refSig} after erasure""".stripMargin)
        assert(ref.signature == ref.denot.signature,
          s"""Before erasure, the signature of a TypeRef should be the signature of its denotation,
             |but the cached signature of $ref was:
             |${ref.signature}, whereas its denotation signature at typer was:
             |${ref.denot.signature}""".stripMargin)
      }
    }
