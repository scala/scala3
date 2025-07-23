package dotty.tools

import vulpix.TestConfiguration

import org.junit.Assert._
import org.junit.Test

import dotc.ast.untpd
import dotc.core.Decorators._
import dotc.core.Contexts._
import dotc.core.Flags._
import dotc.core.Phases._
import dotc.core.Names._
import dotc.core.Types._
import dotc.core.Symbols._
import dotc.core.StdNames._
import dotc.core.Signature
import dotc.typer.ProtoTypes.constrained
import dotc.typer.Inferencing.isFullyDefined
import dotc.typer.ForceDegree
import dotc.util.NoSourcePosition

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

  /** Ensure that signature computation returns an underdefined signature when
   *  the signature depends on uninstantiated type variables.
   */
  @Test def underdefined: Unit =
    inCompilerContext(TestConfiguration.basicClasspath, separateRun = false,
      """trait Foo
        |trait Bar
        |class A[T <: Tuple]:
        |  def and(x: T & Foo): Unit = {}
        |  def andor(x: (T | Bar) & Foo): Unit = {}
        |  def array(x: Array[(T | Bar) & Foo]): Unit = {}
        |  def tuple(x: Foo *: T): Unit = {}
        |  def tuple2(x: Foo *: (T | Tuple) & Foo): Unit = {}
        |""".stripMargin):
      val cls = requiredClass("A")
      val tvar = constrained(cls.requiredMethod(nme.CONSTRUCTOR).info.asInstanceOf[TypeLambda]).head
      tvar <:< defn.TupleTypeRef
      val prefix = cls.typeRef.appliedTo(tvar)

      def checkSignatures(expectedIsUnderDefined: Boolean)(using Context): Unit =
        for decl <- cls.info.decls.toList if decl.is(Method) && !decl.isConstructor do
          val meth = decl.asSeenFrom(prefix)
          val sig = meth.info.signature
          val what = if expectedIsUnderDefined then "underdefined" else "fully-defined"
          assert(sig.isUnderDefined == expectedIsUnderDefined, i"Signature of `$meth` with prefix `$prefix` and type `${meth.info}` should be $what but is `$sig`")

      checkSignatures(expectedIsUnderDefined = true)
      assert(isFullyDefined(tvar, force = ForceDegree.all), s"Could not instantiate $tvar")
      checkSignatures(expectedIsUnderDefined = false)

  /** Check that signature caching behaves correctly with respect to retracted
   *  instantiations of type variables.
   */
  @Test def cachingWithRetraction: Unit =
    inCompilerContext(TestConfiguration.basicClasspath, separateRun = false,
      """trait Foo
        |trait Bar
        |class A[T]:
        |  def and(x: T & Foo): Unit = {}
        |""".stripMargin):
      val cls = requiredClass("A")
      val tvar = constrained(cls.requiredMethod(nme.CONSTRUCTOR).info.asInstanceOf[TypeLambda]).head
      val prefix = cls.typeRef.appliedTo(tvar)
      val ref = prefix.select(cls.requiredMethod("and")).asInstanceOf[TermRef]

      /** Check that the signature of the first parameter of `ref` is equal to `expectedParamSig`. */
      def checkParamSig(ref: TermRef, expectedParamSig: TypeName)(using Context): Unit =
        assertEquals(i"Check failed for param signature of $ref",
          expectedParamSig, ref.signature.paramsSig.head)
        // Both NamedType and MethodOrPoly cache signatures, so check both caches.
        assertEquals(i"Check failed for param signature of ${ref.info} (but not for $ref itself)",
          expectedParamSig, ref.info.signature.paramsSig.head)
        

      // Initially, the param signature is Uninstantiated since it depends on an uninstantiated type variable
      checkParamSig(ref, tpnme.Uninstantiated)

      // In this context, the signature is the erasure of `Bar & Foo`.
      inContext(ctx.fresh.setNewTyperState()):
        tvar =:= requiredClass("Bar").typeRef
        assert(isFullyDefined(tvar, force = ForceDegree.all), s"Could not instantiate $tvar")
        checkParamSig(ref, "Bar".toTypeName)

      // If our caching logic is working correctly, we should get the original signature here.
      checkParamSig(ref, tpnme.Uninstantiated)
