package dotty.tools.bot.util

import org.http4s._
import scalaz.concurrent.Task
import org.http4s.headers.{ Accept, Authorization }

object HttpClientAux {
  def uriFromString(url: String): Task[Uri] =
    Uri.fromString(url).fold(Task.fail, Task.now)

  implicit class RicherTask(val task: Task[Request]) extends AnyVal {
    def withOauth2(token: String): Task[Request] =
      task.map(_.putHeaders(new Authorization(new OAuth2BearerToken(token))))

    def withAuth(user: String, pass: String): Task[Request] =
      task.map(_.putHeaders(new Authorization(BasicCredentials(user, pass))))
  }

  private[this] lazy val previewAcceptHeader =
    Accept.parse("application/vnd.github.black-cat-preview+json")
      .getOrElse(throw new Exception("Couldn't initialize accept header"))

  @inline private def request(method: Method, endpoint: Uri): Request =
    Request(uri = endpoint, method = method)
      .putHeaders(previewAcceptHeader)

  def get(endpoint: String): Task[Request] =
    uriFromString(endpoint).map(request(Method.GET, _))

  def post(endpoint: String): Task[Request] =
    uriFromString(endpoint).map(request(Method.POST, _))

  def delete(endpoint: String): Task[Request] =
    uriFromString(endpoint).map(request(Method.DELETE, _))
}
