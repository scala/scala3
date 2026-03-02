//> using options -source 3.5
trait Eq[A]
trait Order[A] extends Eq[A]:
  def toOrdering: Ordering[A]

def f[Element: Eq: Order] = summon[Eq[Element]].toOrdering // ok

def Test() =
  val eq: Eq[Int] = ???
  val ord: Order[Int] = ???
  f(eq, ord)  // error
  f(using eq, ord) // ok

