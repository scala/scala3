import scala.quoted._
import scala.quoted.Liftable._

object Test {

  '{ ~implicitly[Liftable[Int]].toExpr(1) }

  {
    import Liftable._

    '{ ~IntIsLiftable.toExpr(1) }

    '{ ~1.toExpr }

  }


}