package dotty.tools
package bot
package model

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._

import org.http4s._
import org.http4s.circe._
import org.http4s.client.Client

import scalaz.concurrent.Task
import bot.util.HttpClientAux

object Drone {
  import HttpClientAux._

  case class Build(
    number: Int,
    event: String,
    status: String,
    commit: String,
    author: String
  )

  private[this] val baseUrl = "http://dotty-ci.epfl.ch/api"

  private def job(id: Int) =
    s"$baseUrl/repos/lampepfl/dotty/builds/$id"

  private def job(id: Int, subId: Int) =
    s"$baseUrl/repos/lampepfl/dotty/builds/$id/$subId"

  def stopBuild(id: Int, token: String)(implicit client: Client): Task[Boolean] = {
    def resToBoolean(res: Response): Task[Boolean] = Task.now {
      res.status.code >= 200 && res.status.code < 400
    }

    val responses = List(1, 2, 3, 4).map { subId =>
      client.fetch(delete(job(id, subId)).withOauth2(token))(resToBoolean)
    }

    Task.gatherUnordered(responses).map(xs => xs.exists(_ == true))
  }

  def startBuild(id: Int, token: String)(implicit client: Client): Task[Build] =
    client.expect(post(job(id)).withOauth2(token))(jsonOf[Build])
}
