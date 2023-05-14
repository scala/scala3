import scala.quoted.*

object Places:
  def withPlace[T: Type, R: Type](expr: Expr[T])(body: Quotes ?=> Place[T] => Expr[R])(using Quotes): Expr[R] = {
    // TODO: support applyDynamic/updateDynamic?
    import quotes.reflect.*
    expr match
      case '{ $expr: T } =>
        expr.asTerm match
          case placeVar: Ident if placeVar.symbol.flags.is(Flags.Mutable) =>
            body(
              new Place[T]:
                def set(x: Expr[T]): Expr[Unit] = Assign(placeVar, x.asTerm).asExprOf[Unit]
                def get: Expr[T] = expr
            )
          case Apply(placeApply @ Select(placeTerm, "apply"), List(indexTerm)) =>
            def updateWithCorrectSignature(sym: Symbol): Boolean =
              true // TODO check signature
            val updateSym = placeTerm.symbol.methodMember("update")
              .find(updateWithCorrectSignature)
              .getOrElse(report.errorAndAbort("Cannot assign to " + expr, expr))
            val placeUpdate = placeTerm.select(updateSym)
            indexTerm.asExpr match
              case '{ $indexExpr: idx } =>
                '{
                  val index: idx = $indexExpr
                  ${
                    val boundIndex = '{index}.asTerm
                    val place = new Place[T]:
                      def set(x: Expr[T]): Expr[Unit] = placeUpdate.appliedTo(boundIndex, x.asTerm).asExprOf[Unit]
                      def get: Expr[T] = placeApply.appliedTo(boundIndex).asExprOf[T]
                    body(place)
                  }
                }
          case tree =>
            throw new MatchError(tree.show(using Printer.TreeStructure))
  }


  trait Place[T]:
    def set(x: Expr[T]): Expr[Unit]
    def get: Expr[T]

end Places

inline def increment(inline x: Int): Unit = ${ incrementExpr('x) } // TODO generalize to Numeric types

private def incrementExpr(x: Expr[Int])(using Quotes): Expr[Unit] =
  Places.withPlace(x) { place => place.set('{ ${place.get} + 1 }) }
