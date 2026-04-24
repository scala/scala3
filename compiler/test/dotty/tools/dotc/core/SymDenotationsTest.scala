package dotty.tools.dotc.core

import dotty.tools.{DottyTest, assertThrows}
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.core.SymDenotations.*
import dotty.tools.dotc.core.Symbols.*

import org.junit.Assert.assertFalse
import org.junit.Test

class SymDenotationsTest extends DottyTest:

  @Test def clearTouchedAfterUnexpectedCompletionFailure =
    val sym = newSymbol(defn.RootClass, "boom".toTermName, EmptyFlags, new LazyType:
      def complete(denot: SymDenotation)(using Context): Unit =
        throw new RuntimeException("boom")
    )

    def assertBoom() =
      assertThrows[RuntimeException](_.getMessage == "boom") {
        sym.info
      }

    assertBoom()
    assertFalse(sym.denot.isCompleting)
    assertBoom()
    assertFalse(sym.denot.isCompleting)
