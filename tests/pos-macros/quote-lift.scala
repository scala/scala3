import scala.quoted._

class Test(using QuoteContext) {

  '{ ${implicitly[Liftable[Int]].toExpr(1)} }

  {
    import Liftable._

    '{ ${summon[Liftable[Int]].toExpr(1)} }

    '{ ${Expr(1)} }

  }


}