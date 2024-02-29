import scala.quoted.*

object Macro {
  inline def repeated = ${Macro.repeatedImpl}
  def repeatedImpl(using Quotes):Expr[List[Int]] = {
    import quotes.reflect.*
    val args = List(Expr(1), Expr(2))
    val listObjectTerm = '{ List }.asTerm
    Apply(
      TypeApply(
        Select.unique(listObjectTerm, "apply"),
        List(TypeTree.of[Int])
      ),
      List(
        Typed(
          Repeated(args.map(_.asTerm), TypeTree.of[Int]),
          Inferred(defn.RepeatedParamClass.typeRef.appliedTo(TypeRepr.of[Int]))))
    ).asExprOf[List[Int]]
  }
}