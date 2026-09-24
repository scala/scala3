package dotty.tools
package dotc
package core

import Contexts.*, Types.*, Decorators.*
import ast.tpd

import org.junit.Test
import org.junit.Assert.*

/** Tests for the reduction context footprint of match types.
 *
 *  The footprint is the set of types a cached match type reduction is
 *  revalidated against on every call to `MatchType.reduced`, so it has to be
 *  large enough to catch everything that can invalidate the reduction, and
 *  small enough that revalidating stays cheap.
 */
class MatchTypeFootprintTest extends DottyTest:

  private val source =
    """|class Outer[A, B]:
       |  type Stuck
       |
       |  type PatternParam = Stuck match
       |    case A => Int
       |
       |  type BodyParam = Stuck match
       |    case Int => B
       |
       |  type BodyParamRef = [X] =>> Stuck match
       |    case Int => X
       |""".stripMargin

  /** The footprint of the match type defined by type member `name`, after
   *  reducing it once.
   */
  private def footprintOf(name: String)(op: List[Type] => Unit): Unit =
    checkCompile("typer", source) { (tree, context) =>
      given Context = context

      def matchTypeIn(tp: Type): MatchType = tp match
        case tp: AliasingBounds => matchTypeIn(tp.alias)
        case tp: HKTypeLambda   => matchTypeIn(tp.resType)
        case tp: MatchType      => tp
        case tp                 => fail(s"not a match type: ${tp.show}"); ???

      var found: MatchType | Null = null
      new tpd.TreeTraverser {
        def traverse(t: tpd.Tree)(using Context): Unit =
          t match
            case td: tpd.TypeDef if td.name.toString == name =>
              found = matchTypeIn(td.symbol.info)
            case _ =>
          traverseChildren(t)
      }.traverse(tree)

      assertNotNull(s"no type member named $name", found)
      val mt = found.nn
      // The footprint is computed when a cached reduction is first
      // revalidated, i.e. from the second call on.
      mt.reduced
      mt.reduced
      op(mt.reductionFootprint)
    }

  /** A pattern decides whether its case is selected, so the type parameters it
   *  mentions - whose GADT bounds can change - belong to the footprint.
   */
  @Test def patternTypeParamsAreTracked(): Unit =
    footprintOf("PatternParam") { footprint =>
      assertTrue(
        s"expected A in footprint, got ${footprint.map(_.show)}",
        footprint.exists(_.typeSymbol.name.toString == "A"))
    }

  /** A case body cannot change which case is selected, so the type parameters
   *  it mentions must stay out of the footprint. Tracking them would make
   *  every cache hit cost time proportional to the size of the body - see
   *  bench-micro/tests/matchTypeHeavyBody.scala.
   */
  @Test def bodyTypeParamsAreNotTracked(): Unit =
    footprintOf("BodyParam") { footprint =>
      assertEquals(
        s"expected an empty footprint, got ${footprint.map(_.show)}",
        Nil, footprint)
    }

  /** A parameter reference in a case body does end up in the reduced type, so
   *  it has to be tracked: the typer state can still constrain or instantiate
   *  it, which makes the cached reduction stale.
   */
  @Test def bodyParamRefsAreTracked(): Unit =
    footprintOf("BodyParamRef") { footprint =>
      assertTrue(
        s"expected a TypeParamRef in footprint, got ${footprint.map(_.show)}",
        footprint.exists(_.isInstanceOf[TypeParamRef]))
    }
