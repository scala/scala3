object nullless {
  trait LowerBound[T] {
    type M >: T;
  }
  trait UpperBound[U] {
    type M <: U;
  }
  lazy val nothing : Nothing = nothing
  class Box[V](val v: V)
  lazy val box : Box[UpperBound[String] & LowerBound[Int]] = new Box(nothing)
  def upcast(t : box.v.M) : box.v.M = t // error // error under -strict
  def main(args : Array[String]) : Unit = {
    val zero : String = upcast(0)
    println("...")
  }
}
object bar {
  trait Sub {
    type M
    type L <: M
    type U >: M
    type M2 >: L <: U
  }
  class Box[V](val v: V)

  class Caster[LL, UU] {
    trait S2 {
      type L = LL
      type U = UU
    }
    final lazy val nothing: Nothing = nothing
    final lazy val sub: S2 with Sub = nothing
    final lazy val box : Box[S2 with Sub] = new Box(nothing)
    def upcast(t: box.v.M2): box.v.M2 = t // error // error under -strict
  }
  def main(args : Array[String]) : Unit = {
    val zero : String = (new Caster[Int,String]()).upcast(0)
    println("...")
  }
}
