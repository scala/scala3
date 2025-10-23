import scala.language.`3.8-migration`

class A
class B extends A
class C extends B

class R1
class R2
class R3

// The alternatives are ordered from most genereal to most specific in each test,
// with respect to a lexicographic ordering by parameter list.


object Test1:
  def f(x: A)(y: C) = new R1
  def f(x: B)(y: A) = new R2
  def f(x: B)(y: B) = new R3

  val r = f(new B)(new C) // resolves to: R1 in 3.7, R3 in 3.8
  val _: R3 = r
end Test1


object Test2:
  // R1 is the only applicable alternative in both parts
  // but it is never resolved to since R2 has a more specific 1st parameter list

  object Part1:
    def f(x: A)(y: A) = new R1
    def f(x: B)(y: B) = new R2

    val r = f(new B)(new A) // error, since resolves to R2 in both 3.7 and 3.8, as expected

  object Part2:
    def f(x: A)(y: A) = new R1
    def f(x: B)(y: B) = new R2
    def f(x: B)(y: C) = new R3

    val r = f(new B)(new A) // error since resolves to R2 in 3.8 as in Part1 (was R1 in 3.7)

end Test2
