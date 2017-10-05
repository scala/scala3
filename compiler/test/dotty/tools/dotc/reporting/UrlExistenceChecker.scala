package dotty.tools.dotc.reporting

import java.net.{HttpURLConnection, SocketTimeoutException, URL, UnknownHostException}
import java.util.concurrent.atomic.AtomicBoolean

import org.junit.Assert.fail

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal

/** Checks URL existence by HTTP requests expecting OK responses (200).
  *
  * To not fail if no network is available all URL tests are cancelled if in case
  * "https://www.github.com/" is not available.
  *
  * URL testing can be switched off via system property `linkChecking=false`.
  */
trait UrlExistenceChecker {

  private[this] val runChecks = new AtomicBoolean(true)
  private[this] val httpOk = 200

  private[this] var checks: List[Future[String]] = Nil
  private[this] val urlTimeout = 2000 //ms
  private[this] val systemProperty = "linkChecking" //ms

  /** Tries to connect to "https://www.github.com/" and cancels URL testing if
    * unavailable.
    * Checks system property `linkChecking` to contain `true`.
    */
  def checkForConnectivity(): Unit =
    if (sys.props.getOrElse(systemProperty, "true") == "true")
      Future {
        try {
          if (connectTo("https://www.github.com/") != httpOk) noChecking()
        } catch {
          case NonFatal(_) =>
            noChecking()
        }
      }
    else {
      runChecks.set(false)
      println(s"Warning: URL checking has been switched off via system property `$systemProperty`.")
    }

  def checkLinks(message: String, links: List[String]): Unit =
    if (runChecks.get())
      links.foreach(url =>
        checks = checkUrl(message, url) :: checks)

  def checkUrlResponses(): Unit =
    if (runChecks.get()) {
      import scala.concurrent.duration.DurationInt
      val futureSeq = Future.sequence(checks)
      val res = Await.result(futureSeq, 5.seconds)
      val resStr = res.filter(_.nonEmpty)
      if (resStr.nonEmpty) {
        fail(
          s"""Some URLs did not get response OK (200):
             |  ${resStr.mkString("\n  ")}
             |(Switch URL checking off in sbt via `set javaOptions in Test += "-DlinkChecking=false"`)""".stripMargin)
      }
    }

  private def checkUrl(message: String, url: String) = Future {
    try {
      if (connectTo(url) != httpOk)
        s"$message: No OK response from $url"
      else ""
    } catch {
      case _: SocketTimeoutException =>
        s"$message: Connect timeout for $url"
      case _: UnknownHostException =>
        s"$message: Unknown host for $url"
      case NonFatal(e) =>
        s"$message: $e for $url"
    }
  }

  private def connectTo(url: String): Int = {
    val conn = new URL(url).openConnection().asInstanceOf[HttpURLConnection]
    conn.setRequestMethod("HEAD")
    conn.setConnectTimeout(urlTimeout)
    conn.setReadTimeout(urlTimeout)
    conn.getResponseCode
  }

  private def noChecking(): Unit = {
    runChecks.set(false)
    println("Warning: URL checking has been switched off as github.com can't be reached.")
  }
}
