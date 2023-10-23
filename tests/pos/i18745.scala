object Color:
  def apply(i: Int): Int = i

type Plane

object Plane:
  extension (plane: Plane)
    def zipWith(that: String, f: Int => Int): Int = ???
    def zipWith(that: Int, f: Int => Int): Int = ???

import Plane.zipWith

def test(p: Plane) =
  p.zipWith("", (_: Int) => Color(25))