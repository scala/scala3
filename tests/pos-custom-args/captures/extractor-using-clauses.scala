import caps.*

class A

trait Sel:
  type Out

trait Like[R]

// The type of the second using parameter depends on the first one
object Dependent:
  def unapply(x: A)(using s: Sel, l: Like[s.Out]): Option[A] = Some(x)

class Cap extends SharedCapability

// The using parameter is a capability
object WithCap:
  def unapply(x: A)(using c: Cap): Option[A] = Some(x)

def test(a: A)(using Sel { type Out = Int }, Like[Int], Cap): Int = a match
  case Dependent(_) => 1
  case WithCap(_) => 2
  case _ => 0
