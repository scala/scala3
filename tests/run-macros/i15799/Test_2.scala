@main def Test = {
  object X {
    type Alias = String
    opaque type Opaque = String
    type Bound <: String
    opaque type OpaqueBound <: String = String
  }
  import X.*

  println(s"Alias: ${exampleMacro.typeDefRhs[Alias]}")
  println(s"Opaque: ${exampleMacro.typeDefRhs[Opaque]}")
  println(s"Bound: ${exampleMacro.typeDefRhs[Bound]}")
  println(s"OpaqueBound: ${exampleMacro.typeDefRhs[OpaqueBound]}")

  def testParams[T, U <: String, V >: Int](x: Int): Unit =
    println(s"Param T: ${exampleMacro.typeDefRhs[T]}")
    println(s"Param U: ${exampleMacro.typeDefRhs[U]}")
    println(s"Param V: ${exampleMacro.typeDefRhs[V]}")

  testParams(3)
}
