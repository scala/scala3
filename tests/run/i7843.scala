// scalajs: --skip

import scala.concurrent._, duration.*

object Test {
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
  val thunkFuture: Future[() => String] = Future.successful(() => throw new RuntimeException("Whoops"))

  final def shouldTerminate: Future[Unit] =
    thunkFuture.flatMap { thunk =>
      def fold() = {
        var done = false
        while (!done) {
          println(s"done outside try: $done")
          try {
            thunk()
          } catch {
            case _: Throwable =>
              println("setting done to true")
              done = true
              println(s"done in catch clause: $done")
          }
        }
      }

      Future {
        fold()
      }
    }


  def main(args: Array[String]): Unit = {
    Await.result(shouldTerminate, Duration.Inf)
    println("done")
  }
}