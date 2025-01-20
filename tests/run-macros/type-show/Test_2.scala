
object Test {
  import TypeToolbox.*

  def assertEql[A](obt: A, exp: A): Unit =
    assert(obt == exp, s"\nexpected: $exp\nobtained: $obt")

  def main(args: Array[String]): Unit = {
    val x = 5
    assertEql(show[x.type], "x.type")
    assertEql(show[Nil.type], "scala.Nil.type")
    assertEql(show[Int], "scala.Int")
    assertEql(show[Int => Int], "scala.Function1[scala.Int, scala.Int]")
    assertEql(show[(Int, String)], "scala.Tuple2[scala.Int, scala.Predef.String]")
    assertEql(show[[X] =>> X match { case Int => Int }],
      """[X >: scala.Nothing <: scala.Any] =>> X match  {
        |  case scala.Int => scala.Int
        |}""".stripMargin)
    assertEql(showStructure[[X] =>> X match { case Int => Int }],
      """TypeLambda("""+
        """List(X), """+
        """List(TypeBounds("""+
        """TypeRef(ThisType(TypeRef(NoPrefix(), "scala")), "Nothing"), """+
        """TypeRef(ThisType(TypeRef(NoPrefix(), "scala")), "Any"))), """+
        """MatchType("""+
        """TypeRef(TermRef(ThisType(TypeRef(NoPrefix(), "<root>")), "scala"), "Int"), """+ // match type bound
        """ParamRef(binder, 0), """+
        """List("""+
        """MatchCase("""+
        """TypeRef(TermRef(ThisType(TypeRef(NoPrefix(), "<root>")), "scala"), "Int"), """+
        """TypeRef(TermRef(ThisType(TypeRef(NoPrefix(), "<root>")), "scala"), "Int")))))""")

    // TODO: more complex types:
    //  - implicit function types
    //  - dependent function types
    //  - refinement types
  }
}
