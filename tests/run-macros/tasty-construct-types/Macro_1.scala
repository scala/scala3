import quoted.*

object Macros {
  inline def theTestBlock : Unit = ${ theTestBlockImpl }

  trait RefineMe {
    type T
    def foo : T
  }

  class TestAnnotation extends scala.annotation.Annotation

  def theTestBlockImpl(using qctx : Quotes) : Expr[Unit] = {
    import quotes.reflect.*

    val x1T = ConstantType(IntConstant(1))
    val x2T = OrType(ConstantType(IntConstant(1)), ConstantType(IntConstant(2)))
    val x3T = AndType(ConstantType(IntConstant(3)), TypeRepr.of[Any])
    val x4T =
      TypeLambda(
        List("A","B"),
        _ => List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any]), TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])),
        (tl : TypeLambda) => tl.param(1))
    val x5T =
      Refinement(
        TypeRepr.of[RefineMe],
        "T",
        TypeBounds(TypeRepr.of[Int], TypeRepr.of[Int]))
    val x6T = TypeRepr.of[List].appliedTo(List(TypeRepr.of[Int]))
    val x7T = AnnotatedType(ConstantType(IntConstant(7)), '{ new TestAnnotation }.asTerm)
    val x8T =
      MatchType(
        TypeRepr.of[Int],
        TypeRepr.of[List[8]],
        List(
          TypeLambda(
            List("t"),
            _ => List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])),
            tl => MatchCase(TypeRepr.of[List].appliedTo(tl.param(0)), tl.param(0))),
          MatchCase(TypeRepr.of[Int], TypeRepr.of[Int])
        )
      )

    assert(x1T =:= TypeRepr.of[1])
    assert(x2T =:= TypeRepr.of(using Type.of[1|2]))
    assert(x3T =:= TypeRepr.of[3&Any])
    assert(x4T =:= TypeRepr.of[[A,B] =>> B])
    assert(x5T =:= TypeRepr.of[RefineMe { type T = Int }])
    assert(x6T =:= TypeRepr.of[List[Int]])
    assert(x7T =:= TypeRepr.of[7 @TestAnnotation])
    assert(x8T =:= TypeRepr.of[List[8] match {
      case List[t] => t
      case Int => Int
    }])

    '{
      println("Ok")
    }
  }
}
