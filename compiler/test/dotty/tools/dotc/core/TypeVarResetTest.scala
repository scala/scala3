package dotty.tools
package dotc.core

import vulpix.TestConfiguration

import dotty.tools.dotc.core.Contexts.{*, given}
import dotty.tools.dotc.core.Decorators.{*, given}
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.typer.ProtoTypes.constrained

import org.junit.Test
import org.junit.Assert.*

/** Tests for TypeVar.resetInst semantics.
 *
 *  When a TypeVar is reset via resetInst, its mightBeProvisional flag
 *  must be reset to true so that caches based on isProvisional
 *  correctly re-evaluate.
 */
class TypeVarResetTest:

  @Test def resetInstResetsProvisionalFlag: Unit =
    inCompilerContext(TestConfiguration.basicClasspath,
        scalaSources = "trait A { def foo[T]: Any }") {

      // Create a constrained type variable
      val List(tvar) = constrained(requiredClass("A").typeRef.select("foo".toTermName).info.asInstanceOf[TypeLambda])

      // Initially, the TypeVar is uninstantiated and should be provisional
      assertTrue("Uninstantiated TypeVar should be provisional",
        tvar.isProvisional)

      // Now instantiate the TypeVar
      tvar =:= defn.IntType

      // After instantiation with a concrete type, it should not be provisional
      // (assuming no other provisional elements)
      val wasProvisionalAfterInst = tvar.isProvisional

      // Take a snapshot
      val snapshot = ctx.typerState.snapshot()

      // The type variable should now be referenced as instantiated
      assertTrue("TypeVar should be instantiated after =:=",
        tvar.instanceOpt.exists)

      // Now reset to the snapshot (before instantiation was committed)
      ctx.typerState.resetTo(snapshot)

      // After reset, the TypeVar should be provisional again because
      // resetInst sets mightBeProvisional = true
      assertTrue("After resetTo snapshot, TypeVar must be provisional again",
        tvar.isProvisional)
    }
  end resetInstResetsProvisionalFlag

  @Test def containingTypeBecomesProvisionalAfterReset: Unit =
    inCompilerContext(TestConfiguration.basicClasspath,
        scalaSources = "trait A { def foo[T]: Any }") {

      // Create a constrained type variable
      val List(tvar) = constrained(requiredClass("A").typeRef.select("foo".toTermName).info.asInstanceOf[TypeLambda])

      // Create an AppliedType containing the TypeVar: List[T]
      val listOfTvar = defn.ListType.appliedTo(tvar)

      // Initially, the containing type should be provisional (contains uninstantiated TypeVar)
      assertTrue("AppliedType containing uninstantiated TypeVar should be provisional",
        listOfTvar.isProvisional)

      // Take a snapshot before instantiation
      val snapshot = ctx.typerState.snapshot()

      // Instantiate the TypeVar
      tvar =:= defn.IntType

      // The TypeVar should be marked as instantiated in the constraint
      assertTrue("TypeVar should have an instance after =:=",
        ctx.typerState.constraint.instType(tvar).exists)

      // Reset to snapshot (before instantiation)
      ctx.typerState.resetTo(snapshot)

      // After reset, the containing type should be provisional again
      // This works because resetInst sets mightBeProvisional = true on the TypeVar,
      // which causes isProvisional to re-evaluate
      assertTrue("After reset, containing type must be provisional again",
        listOfTvar.isProvisional)
    }
  end containingTypeBecomesProvisionalAfterReset
