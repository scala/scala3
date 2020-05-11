import tasty._
import quoted._

object Macros {
  inline def theTestBlock : Unit = ${ theTestBlockImpl }

  trait RefineMe {
    type T
    def foo : T
  }

  class TestAnnotation extends scala.annotation.Annotation

  def theTestBlockImpl(using s: Scope): s.Expr[Unit] = {
    import s.tasty._

    val x1T = ConstantType(Constant(1))
    val x2T = OrType(ConstantType(Constant(1)), ConstantType(Constant(2)))
    val x3T = AndType(ConstantType(Constant(3)), Type.of[Any])
    val x4T =
      TypeLambda(
        List("A","B"),
        _ => List(TypeBounds(Type.of[Nothing], Type.of[Any]), TypeBounds(Type.of[Nothing], Type.of[Any])),
        (tl : TypeLambda) => tl.param(1))
    val x5T =
      Refinement(
        Type.of[RefineMe],
        "T",
        TypeBounds(Type.of[Int], Type.of[Int]))
    val x6T = Type.of[List].appliedTo(List(Type.of[Int]))
    val x7T = AnnotatedType(ConstantType(Constant(7)), '{ new TestAnnotation })
    val x8T =
      MatchType(
        Type.of[Int],
        Type.of[List[8]],
        List(
          TypeLambda(
            List("t"),
            _ => List(TypeBounds(Type.of[Nothing], Type.of[Any])),
            tl => Type.of[scala.internal.MatchCase].appliedTo(List(Type.of[List].appliedTo(tl.param(0)), tl.param(0)))))
      )

    assert(x1T =:= '[1].tpe)
    assert(x2T =:= '[1|2].tpe)
    assert(x3T =:= '[3&Any].tpe)
    assert(x4T =:= '[[A,B] =>> B].tpe)
    assert(x5T =:= '[RefineMe { type T = Int }].tpe)
    assert(x6T =:= '[List[Int]].tpe)
    assert(x7T =:= '[7 @TestAnnotation].tpe)
    assert(x8T =:= '[List[8] match { case List[t] => t }].tpe)

    '{
      println("Ok")
    }
  }
}

