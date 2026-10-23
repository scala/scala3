// Inspired from https://goto.ucsd.edu/~rjhala/liquid/abstract_refinement_types.pdf
//

trait Ordering[T]:
  def compare(x: T, y: T): Int

given Ordering[Int]:
  def compare(x: Int, y: Int): Int =
    if x < y then -1 else if x > y then 1 else 0

def max[T: Ordering as ord, U <: T](x: U, y: U): U =
  if ord.compare(x, y) >= 0 then x else y

// Example 1 from OOPSLA 26
def maximum[T: Ordering, U <: T](xs: List[U]): U = xs.reduce(max)
type Even = {v: Int with v % 2 == 0}
def test: Even = maximum(List(2, 4, 6))
