import scala.language.`3.7-migration`

class A
class B extends A
class C extends B

class R1
class R2
class R3

// In each test, the alternatives are defined from most genereal to most specific,
// with respect to a lexicographic ordering by parameter list.
// In each of the cases tested here, scala-3.7 resolves to the most specific applicable alternative.
// See tests/neg/multiparamlist-overload-3.6.scala for the comparison.

object Test1:
  def f(x: A)(y: C) = new R1
  def f(x: B)(y: A) = new R2
  def f(x: B)(y: B) = new R3

  val r = f(new B)(new C) // warn: all alternatives are applicable; resolves to: R1 in 3.6, R3 in 3.7
  val _: R3 = r
end Test1

object Test2:
  // R1 is the only applicable alternative in both parts
  // but (in 3.7) it is only resolved to in Part1 by adding (an unapplicable) R3

  object Part1:
    def f(x: A)(y: A) = new R1
    def f(x: B)(y: B) = new R2
    def f(x: B)(y: C) = new R3

    val r = f(new B)(new A) // resolves to: R1 in 3.6, R1 in 3.7
    val _: R1 = r

  object Part2:
    def f(x: A)(y: A) = new R1
    def f(x: B)(y: B) = new R2

    val r = f(new B)(new A) // warn: resolves to: R2 in 3.6, R1 in 3.7
    val _: R1 = r

end Test2
