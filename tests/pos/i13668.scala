class MyType()
trait Candidate[R]
given Candidate[MyType] with {}
class Fuzzy[W]()
class Fuzzy1()
class Bear()

extension [L](lhs: L)(using Candidate[L])
  def +[RW](rhs: Fuzzy[RW]): Unit = {}
  def +(rhs: Bear): Unit = {}
  def -(rhs: Fuzzy1): Unit = {}
  def -(rhs: Bear): Unit = {}

val works = MyType() - Fuzzy1()
val fails = MyType() + Fuzzy[1]()
