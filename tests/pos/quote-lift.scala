import scala.quoted._

object Test {

  implicit def dummy: StagingContext = ???

  '{ ${implicitly[Liftable[Int]].toExpr(1)} }

  {
    import Liftable._

    '{ ${IntIsLiftable.toExpr(1)} }

    '{ ${1.toExpr} }

  }

}
