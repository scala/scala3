package dotty.tools
package dotc
package core

import vulpix.TestConfiguration

import Contexts.{*, given}, Decorators.{*, given}, Symbols.*, Types.*
import ast.tpd.*
import typer.ProtoTypes.*
import util.SimpleIdentitySet

import org.junit.Test

class ConstraintsTest:

  @Test def mergeParamsTransitivity: Unit =
    inCompilerContext(TestConfiguration.basicClasspath,
        scalaSources = "trait A { def foo[S, T, R]: Any  }") {
      val tvars = constrained(requiredClass("A").typeRef.select("foo".toTermName).info.asInstanceOf[TypeLambda], EmptyTree, alwaysAddTypeVars = true)._2
      val List(s, t, r) = tvars.tpes

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
      val tvars = constrained(requiredClass("A").typeRef.select("foo".toTermName).info.asInstanceOf[TypeLambda], EmptyTree, alwaysAddTypeVars = true)._2
      val List(s, t) = tvars.tpes

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

  @Test def validBoundsInit: Unit = inCompilerContext(
    TestConfiguration.basicClasspath,
    scalaSources = "trait A { def foo[S >: T <: T | Int, T <: String]: Any  }") {
      val tvars = constrained(requiredClass("A").typeRef.select("foo".toTermName).info.asInstanceOf[TypeLambda], EmptyTree, alwaysAddTypeVars = true)._2
      val List(s, t) = tvars.tpes

      val TypeBounds(lo, hi) = ctx.typerState.constraint.entry(t.asInstanceOf[TypeVar].origin): @unchecked
      assert(lo =:= defn.NothingType, i"Unexpected lower bound $lo for $t: ${ctx.typerState.constraint}")
      assert(hi =:= defn.StringType, i"Unexpected upper bound $hi for $t: ${ctx.typerState.constraint}") // used to be Any
  }

  @Test def validBoundsUnify: Unit = inCompilerContext(
    TestConfiguration.basicClasspath,
    scalaSources = "trait A { def foo[S >: T <: T | Int, T <: String | Int]: Any  }") {
      val tvars = constrained(requiredClass("A").typeRef.select("foo".toTermName).info.asInstanceOf[TypeLambda], EmptyTree, alwaysAddTypeVars = true)._2
      val List(s, t) = tvars.tpes

      s <:< t

      val TypeBounds(lo, hi) = ctx.typerState.constraint.entry(t.asInstanceOf[TypeVar].origin): @unchecked
      assert(lo =:= defn.NothingType, i"Unexpected lower bound $lo for $t: ${ctx.typerState.constraint}")
      assert(hi =:= (defn.StringType | defn.IntType), i"Unexpected upper bound $hi for $t: ${ctx.typerState.constraint}")
  }

  @Test def validBoundsReplace: Unit = inCompilerContext(
    TestConfiguration.basicClasspath,
    scalaSources = "trait X; trait A { def foo[S <: U | X, T, U]: Any }") {
      val tvarTrees = constrained(requiredClass("A").typeRef.select("foo".toTermName).info.asInstanceOf[TypeLambda], EmptyTree, alwaysAddTypeVars = true)._2
      val tvars @ List(s, t, u) = tvarTrees.tpes.asInstanceOf[List[TypeVar]]
      s =:= t
      t =:= u

      for tvar <- tvars do
        val entry = ctx.typerState.constraint.entry(tvar.origin)
        assert(!ctx.typerState.constraint.occursAtToplevel(tvar.origin, entry),
          i"cyclic bound for ${tvar.origin}: ${entry} in ${ctx.typerState.constraint}")
  }

  @Test def splitIntersectedBounds: Unit = inCompilerContext(TestConfiguration.basicClasspath) {
    val Foo = newTypeVar(TypeBounds.empty, "Foo".toTypeName)
    val Bar = newTypeVar(TypeBounds.empty, "Bar".toTypeName)
    val Int = defn.IntType

    val tp1 = Foo & Int
    val tp2 = Bar & Int
    val log = TypeComparer.explained(_.isSameType(tp1, tp2))
    //println(i"$log")
    //println(i"${ctx.typerState.constraint}")

    val tree = ref(newAnonFun(defn.RootClass, MethodType(Nil, defn.UnitType))).appliedToNone
    ctx.typer.interpolateTypeVars(tree, WildcardType, SimpleIdentitySet.empty)
    assert(Foo =:= Int && Bar =:= Int, i"Foo=$Foo Bar=$Bar")
  }
