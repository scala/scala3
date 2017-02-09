package dotty.tools.bot

import org.http4s.server.{ Server, ServerApp }
import org.http4s.server.blaze._

import scalaz.concurrent.Task

object Main extends ServerApp with PullRequestService {

  val user = sys.env("USER")
  val token = sys.env("TOKEN")

  /** Services mounted to the server */
  final val services = prService

  override def server(args: List[String]): Task[Server] = {
    BlazeBuilder
      .bindHttp(8080, "localhost")
      .mountService(services, "/api")
      .start
  }
}
