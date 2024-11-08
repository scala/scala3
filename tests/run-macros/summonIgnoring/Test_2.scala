//> using options -experimental

@main def Test(): Unit = {
  class C2
  println("No given in scope:")
  summon[TC[C2]].print()

  {
    println("Given in scope:")
    given TC[C1] = new TC[C1] {
      def print() = println("TC[C1] defined by a user")
    }
    summon[TC[C2]].print()
  }
}