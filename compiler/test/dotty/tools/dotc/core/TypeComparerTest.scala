package dotty.tools
package dotc
package core

import Contexts.*, Decorators.*, Denotations.*, SymDenotations.*, Symbols.*, Types.*
import printing.Formatting.Show

import org.junit.Test
import org.junit.Assert.*

class TypeComparerTest extends DottyTest:
  val LongType = defn.LongType

  // Ensure glb and lub give lower and upper bounds when one of the inputs is WildcardType
  // and that glb and lub honours left identity and right identity, and thus is commutative with WildcardType
  @Test def glbWildcardL = identityL("glb", glb)(LongType, id = WildcardType)
  @Test def glbWildcardR = identityR("glb", glb)(LongType, id = WildcardType)
  @Test def lubWildcardL = identityL("lub", lub)(LongType, id = WildcardType)
  @Test def lubWildcardR = identityR("lub", lub)(LongType, id = WildcardType)

  def identityL[A: Show](op: String, fn: (A, A) => A)(a: A, id: A) =
    val x = fn(id, a)
    assertEquals(i"$op(id=$id, $a) = $x, expected $a (left identity)", a, x)

  def identityR[A: Show](op: String, fn: (A, A) => A)(a: A, id: A) =
    val x = fn(a, id)
    assertEquals(i"$op($a, id=$id) = $x, expected $a (right identity)", a, x)

  // glb(a, b) = x such that x <: a, x <: b, & forAll y, y <: a, y <: b ==> y <: x
  def glb(a: Type, b: Type) =
    val x = TypeComparer.glb(a, b)
    assertTrue(i"glb($a, $b) = $x, but $x !<: $a", x <:< a)
    assertTrue(i"glb($a, $b) = $x, but $x !<: $b", x <:< b)
    x

  // lub(a, b) = x such that a <: x, b <: x, & forAll y, a <: y, b <: y ==> x <: y
  def lub(a: Type, b: Type) =
    val x = TypeComparer.lub(a, b)
    assertTrue(i"lub($a, $b) = $x, but $a !<: $x", a <:< x)
    assertTrue(i"lub($a, $b) = $x, but $b !<: $x", b <:< x)
    x
end TypeComparerTest
