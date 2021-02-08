import scala.quoted.*

class Test(using Quotes) {

  '{ ${implicitly[ToExpr[Int]].apply(1)} }

  {
    import ToExpr.*

    '{ ${summon[ToExpr[Int]].apply(1)} }

    '{ ${Expr(1)} }

  }


}