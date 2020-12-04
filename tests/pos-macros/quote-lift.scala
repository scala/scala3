import scala.quoted._

class Test(using Quotes) {

  '{ ${implicitly[ToExpr[Int]].apply(1)} }

  {
    import ToExpr._

    '{ ${summon[ToExpr[Int]].apply(1)} }

    '{ ${Expr(1)} }

  }


}