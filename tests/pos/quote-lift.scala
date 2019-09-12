import scala.quoted._

object Test {
   given as QuoteContext = ???

  '{ ${implicitly[Liftable[Int]].toExpr(1)} }

  {
    import Liftable._

    '{ ${summon[Liftable[Int]].toExpr(1)} }

    '{ ${1.toExpr} }

  }


}