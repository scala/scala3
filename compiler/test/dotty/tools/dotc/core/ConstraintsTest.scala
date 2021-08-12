package dotty.tools
package dotc.core

import vulpix.TestConfiguration

import dotty.tools.dotc.core.Contexts.{*, given}
import dotty.tools.dotc.core.Decorators.{*, given}
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.typer.ProtoTypes.constrained

import org.junit.Test

import dotty.tools.DottyTest

class ConstraintsTest:

  @Test def mergeParamsTransitivity: Unit =
    inCompilerContext(TestConfiguration.basicClasspath,
        scalaSources = "trait A { def foo[S, T, R]: Any  }") {
      val tp = constrained(requiredClass("A").typeRef.select("foo".toTermName).info.asInstanceOf[TypeLambda])
      val List(s, t, r) = tp.paramRefs

      val innerCtx = ctx.fresh.setExploreTyperState()
      inContext(innerCtx) {
        s <:< t
      }

      t <:< r

      ctx.typerState.mergeConstraintWith(innerCtx.typerState)
      assert(s frozen_<:< r,
        i"Merging constraints `?S <: ?T` and `?T <: ?R` should result in `?S <:< ?R`: ${ctx.typerState.constraint}")
    }
  end mergeParamsTransitivity

  @Test def mergeBoundsTransitivity: Unit =
    inCompilerContext(TestConfiguration.basicClasspath,
        scalaSources = "trait A { def foo[S, T]: Any  }") {
      val tp = constrained(requiredClass("A").typeRef.select("foo".toTermName).info.asInstanceOf[TypeLambda])
      val List(s, t) = tp.paramRefs

      val innerCtx = ctx.fresh.setExploreTyperState()
      inContext(innerCtx) {
        s <:< t
      }

      defn.IntType <:< s

      ctx.typerState.mergeConstraintWith(innerCtx.typerState)
      assert(defn.IntType frozen_<:< t,
        i"Merging constraints `?S <: ?T` and `Int <: ?S` should result in `Int <:< ?T`: ${ctx.typerState.constraint}")
    }
  end mergeBoundsTransitivity
