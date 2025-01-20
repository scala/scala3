//> using options -experimental

@main def Test(): Unit = {
  class C2
  summon[TC[C2]].print()
}