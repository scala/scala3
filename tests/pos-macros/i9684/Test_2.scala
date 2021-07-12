import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps

@main def Test: Unit =
  val latch = Vector.fill(10)(Future successful 10)
  X.printType( { Await.ready(latch(3) , 1 second) } )
