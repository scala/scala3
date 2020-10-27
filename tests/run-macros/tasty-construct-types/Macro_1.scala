import tasty._
import quoted._

object Macros {
  inline def theTestBlock : Unit = ${ theTestBlockImpl }

  trait RefineMe {
    type T
    def foo : T
  }

  class TestAnnotation extends scala.annotation.Annotation

  def theTestBlockImpl(using qctx : QuoteContext) : Expr[Unit] = {
    import qctx.reflect._

    val x1T = ConstantType(Constant.Int(1))
    val x2T = OrType(ConstantType(Constant.Int(1)), ConstantType(Constant.Int(2)))
    val x3T = AndType(ConstantType(Constant.Int(3)), TypeRepr.of[Any])
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
    val x7T = AnnotatedType(ConstantType(Constant.Int(7)), '{ new TestAnnotation }.unseal)
    val x8T =
      MatchType(
        TypeRepr.of[Int],
        TypeRepr.of[List[8]],
        List(
          TypeLambda(
            List("t"),
            _ => List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])),
            tl => TypeRepr.of[scala.internal.MatchCase].appliedTo(List(TypeRepr.of[List].appliedTo(tl.param(0)), tl.param(0)))))
      )

    assert(x1T =:= Type[1].unseal.tpe)
    assert(x2T =:= Type[1|2].unseal.tpe)
    assert(x3T =:= Type[3&Any].unseal.tpe)
    assert(x4T =:= Type[[A,B] =>> B].unseal.tpe)
    assert(x5T =:= Type[RefineMe { type T = Int }].unseal.tpe)
    assert(x6T =:= Type[List[Int]].unseal.tpe)
    assert(x7T =:= Type[7 @TestAnnotation].unseal.tpe)
    assert(x8T =:= Type[List[8] match { case List[t] => t }].unseal.tpe)

    '{
      println("Ok")
    }
  }
}

