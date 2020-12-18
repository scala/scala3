package tests

package specializedSignature

import scala.{specialized}

trait AdditiveMonoid[@specialized(Int, Long, Float, Double) A]
{
  def a: A
   = ???

  def b[@specialized(Int, Float) B]: B
   = ???
}