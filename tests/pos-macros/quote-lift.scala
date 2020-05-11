import scala.quoted._

class Test(using s: Scope) {

  '{ ${implicitly[scope.Liftable[Int]].toExpr(1)} }

  {
    import scope.Liftable._

    '{ ${summon[scope.Liftable[Int]].toExpr(1)} }

    '{ ${Expr(1)} }

  }


}