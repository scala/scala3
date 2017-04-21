// from https://github.com/rickynils/scalacheck/issues/290
import scala.concurrent._
import scala.concurrent.duration._
import java.util.concurrent.Executors

object Test {
  import ExecutionContext.Implicits.global
  val x = Await.result(Future(1), 1000.seconds)
  def main(args: Array[String]) = println(x)
}

