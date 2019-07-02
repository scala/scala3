import scala.quoted._

object Test {
   delegate for QuoteContext = ???

  '{ ${implicitly[Liftable[Int]].toExpr(1)} }

  {
    import Liftable._

    '{ ${the[Liftable[Int]].toExpr(1)} }

    '{ ${1.toExpr} }

  }


}