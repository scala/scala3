// scalajs: --skip

// from https://github.com/rickynils/scalacheck/issues/290
import scala.concurrent.*
import scala.concurrent.duration.*
import java.util.concurrent.Executors

object Test {
  import ExecutionContext.Implicits.global
  val x = Await.result(Future(1), 1000.seconds)
  def main(args: Array[String]) = println(x)
}

