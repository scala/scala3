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
    import qctx.tasty._

    val x1T = ConstantType(Constant(1))
    val x2T = OrType(ConstantType(Constant(1)), ConstantType(Constant(2)))
    val x3T = AndType(ConstantType(Constant(3)), typeOf[Any])
    val x4T =
      TypeLambda(
        List("A","B"),
        _ => List(TypeBounds(typeOf[Nothing], typeOf[Any]), TypeBounds(typeOf[Nothing], typeOf[Any])),
        (tl : TypeLambda) => tl.param(1))
    val x5T =
      Refinement(
        typeOf[RefineMe],
        "T",
        TypeBounds(typeOf[Int], typeOf[Int]))
    val x6T = AppliedType(Type(classOf[List[_]]), List(typeOf[Int]))
    val x7T = AnnotatedType(ConstantType(Constant(7)), '{ new TestAnnotation }.asTerm)
    val x8T =
      MatchType(
        typeOf[Int],
        typeOf[List[8]],
        List(
          TypeLambda(
            List("t"),
            _ => List(TypeBounds(typeOf[Nothing], typeOf[Any])),
            tl => AppliedType(MatchCaseType, List(AppliedType(Type(classOf[List[_]]), List(tl.param(0))), tl.param(0)))))
      )

    assert(x1T =:= '[1].unseal.tpe)
    assert(x2T =:= '[1|2].unseal.tpe)
    assert(x3T =:= '[3&Any].unseal.tpe)
    assert(x4T =:= '[[A,B] =>> B].unseal.tpe)
    assert(x5T =:= '[RefineMe { type T = Int }].unseal.tpe)
    assert(x6T =:= '[List[Int]].unseal.tpe)
    assert(x7T =:= '[7 @TestAnnotation].unseal.tpe)
    assert(x8T =:= '[List[8] match { case List[t] => t }].unseal.tpe)

    '{
      println("Ok")
    }
  }
}

