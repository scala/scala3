//> using options -source future -deprecation -Werror

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
    final lazy val sub: S2 & Sub = nothing
    final lazy val box : Box[S2 & Sub] = new Box(nothing)
    def upcast(t: box.v.M2): box.v.M2 = t  // error // error
  }
  def main(args : Array[String]) : Unit = {
    val zero : String = (new Caster[Int,String]()).upcast(0)
    println("...")
  }
}
