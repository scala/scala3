package dotty.tools
package dotc
package transform
package patmat

import core.*, Annotations.*, Contexts.*, Decorators.*, Flags.*, Names.*, StdNames.*, Symbols.*, Types.*
import ast.*, tpd.*

import vulpix.TestConfiguration, TestConfiguration.basicClasspath

import org.junit, junit.Test, junit.Assert.*

class SpaceEngineTest:
  import SpaceEngine.*

  @Test def isSubspaceTest1: Unit = inCompilerContext(basicClasspath) {
    // Testing the property of `isSubspace` that:
    // isSubspace(a, b)  <=>  simplify(simplify(a) - simplify(a)) == Empty
    // Previously there were no simplify calls,
    // and this is a counter-example,
    // for which you need either to simplify(b) or simplify the minus result.

    val tp      = defn.ConsType.appliedTo(defn.AnyType)
    val unappTp = requiredMethod("scala.collection.immutable.::.unapply").termRef
    val params  = List(Empty, Typ(tp))

    val a = Prod(tp, unappTp, params)
    val b = Empty

    val res1 = isSubspace(a, b)

    val a2   = simplify(a)
    val b2   = simplify(b)
    val rem1 = minus(a2, b2)
    val rem2 = simplify(rem1)
    val res2 = rem2 == Empty

    assertEquals(
      i"""|isSubspace:
          |
          |isSubspace(a, b) = $res1
          |
          |Should be equivalent to:
          |simplify(simplify(a) - simplify(b)) == Empty
          |simplify(a2          - b2)          == Empty
          |simplify(rem1)                      == Empty
          |rem2                                == Empty
          |
          |a    = ${show(a)}
          |b    = ${show(b)}
          |a2   = ${show(a2)}
          |b2   = ${show(b2)}
          |rem1 = ${show(rem1)}
          |rem2 = ${show(rem2)}
          |
          |a    = ${a.toString}
          |b    = ${b.toString}
          |a2   = ${a2.toString}
          |b2   = ${b2.toString}
          |rem1 = ${rem1.toString}
          |rem2 = ${rem2.toString}
          |
          |""".stripMargin, res1, res2)
  }
