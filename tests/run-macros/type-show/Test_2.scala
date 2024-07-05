
object Test {
  import TypeToolbox.*
  def main(args: Array[String]): Unit = {
    val x = 5
    assert(show[x.type] == "x.type")
    assert(show[Nil.type] == "scala.Nil.type")
    assert(show[Int] == "scala.Int")
    assert(show[Int => Int] == "scala.Function1[scala.Int, scala.Int]")
    assert(show[(Int, String)] == "scala.Tuple2[scala.Int, scala.Predef.String]")
    assert(show[[X] =>> X match { case Int => Int }] ==
      """[X >: scala.Nothing <: scala.Any] => X match  {
        |  case scala.Int => scala.Int
        |}""".stripMargin)
    assert(showStructure[[X] =>> X match { case Int => Int }] == """TypeLambda(List(X), List(TypeBounds(TypeRef(ThisType(TypeRef(NoPrefix(), "scala")), "Nothing"), TypeRef(ThisType(TypeRef(NoPrefix(), "scala")), "Any"))), MatchType(TypeRef(ThisType(TypeRef(NoPrefix(), "scala")), "Any"), ParamRef(binder, 0), List(MatchCase(TypeRef(TermRef(ThisType(TypeRef(NoPrefix(), "<root>")), "scala"), "Int"), TypeRef(TermRef(ThisType(TypeRef(NoPrefix(), "<root>")), "scala"), "Int")))))""")

    // TODO: more complex types:
    //  - implicit function types
    //  - dependent function types
    //  - refinement types
  }
}
