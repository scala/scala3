package tests

package specializedSignature

import scala.{specialized}

trait AdditiveMonoid[@specialized(Int, Long, Float, Double) A] //expected: trait AdditiveMonoid[A]
{
  def a: A
   = ???

  def b[@specialized(Int, Float) B]: B //expected: def b[B]: B
   = ???
}
