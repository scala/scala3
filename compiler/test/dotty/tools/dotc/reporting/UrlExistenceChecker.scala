package dotty.tools.dotc.reporting

import java.net.{HttpURLConnection, SocketTimeoutException, URL, UnknownHostException}
import java.util.concurrent.atomic.AtomicBoolean

import org.junit.Assert.fail

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt
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

  private[this] var checkingUrls: Set[String] = Set.empty
  private[this] var checks: List[Future[String]] = Nil
  private[this] val urlTimeout = 10.seconds
  private[this] val urlTimeoutInt: Int = urlTimeout.toMillis.toInt
  private[this] val systemProperty = "linkChecking"

  /** Tries to connect to "https://www.github.com/" and cancels URL testing if
    * unavailable.
    * Checks system property `linkChecking` be undefined or to contain `true`.
    */
  def checkForConnectivity(): Unit = {
    def noChecking(text: String =
        "Warning: URL checking has been switched off as github.com can't be reached."): Unit = {
      runChecks.set(false)
      println(text)
    }

    if (sys.props.getOrElse(systemProperty, "true") == "true")
      Future {
        try {
          if (connectTo("https://www.github.com/", timeout = 2000) != httpOk)
            noChecking()
        } catch {
          case NonFatal(_) =>
            noChecking()
        }
      }
    else
      noChecking(
        s"Warning: URL checking has been switched off via system property `$systemProperty`.")
  }

  def checkLinks(message: String, links: List[String]): Unit =
    if (runChecks.get())
      links.foreach { url =>
        if (!checkingUrls.contains(url)) {
          checkingUrls += url
          checks = checkUrl(message, url) :: checks
        }
      }

  /** Awaits completion of all URL checks and fails the test if any has a non empty
    * String as a result.
    */
  def checkUrlResponses(): Unit =
    if (runChecks.get()) {
      val allFutures = Future.sequence(checks)
      val results = Await.result(allFutures, urlTimeout + 2.seconds).filter(_.nonEmpty)
      if (results.nonEmpty)
        fail(s"""Some URLs did not get response OK (200):
           |  ${results.mkString("\n  ")}
           |(Switch URL checking off in sbt via `set javaOptions in Test += "-DlinkChecking=false"`)""".stripMargin)
    }

  private def checkUrl(message: String, url: String) = Future {
    try {
      val response = connectTo(url)
      if (response != httpOk)
        s"$message: Got HTTP $response response from $url"
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

  private def connectTo(url: String, timeout: Int = urlTimeoutInt): Int = {
    val conn = new URL(url).openConnection().asInstanceOf[HttpURLConnection]
    conn.setRequestMethod("HEAD")
    conn.setConnectTimeout(timeout)
    conn.setReadTimeout(timeout)
    conn.getResponseCode
  }

}
