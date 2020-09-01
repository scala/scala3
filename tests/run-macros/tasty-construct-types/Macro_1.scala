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
    val x6T = Type(classOf[List[_]]).appliedTo(List(typeOf[Int]))
    val x7T = AnnotatedType(ConstantType(Constant(7)), '{ new TestAnnotation }.unseal)
    val x8T =
      MatchType(
        typeOf[Int],
        typeOf[List[8]],
        List(
          TypeLambda(
            List("t"),
            _ => List(TypeBounds(typeOf[Nothing], typeOf[Any])),
            tl => MatchCaseType.appliedTo(List(Type(classOf[List[_]]).appliedTo(tl.param(0)), tl.param(0)))))
      )

    assert(x1T =:= quoted.Type[1].unseal.tpe)
    assert(x2T =:= quoted.Type[1|2].unseal.tpe)
    assert(x3T =:= quoted.Type[3&Any].unseal.tpe)
    assert(x4T =:= quoted.Type[[A,B] =>> B].unseal.tpe)
    assert(x5T =:= quoted.Type[RefineMe { type T = Int }].unseal.tpe)
    assert(x6T =:= quoted.Type[List[Int]].unseal.tpe)
    assert(x7T =:= quoted.Type[7 @TestAnnotation].unseal.tpe)
    assert(x8T =:= quoted.Type[List[8] match { case List[t] => t }].unseal.tpe)

    '{
      println("Ok")
    }
  }
}

