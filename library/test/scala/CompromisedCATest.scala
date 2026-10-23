package scala

import java.io.IOException
import java.net.URI
import javax.net.ssl.{HttpsURLConnection, SSLException}
import org.junit.Assert.assertTrue
import org.junit.Test

class CompromisedCATest:
  private val testUrls = List(
    "https://dv-g3b-emudhra.we-have-a-problem.com/",
    "https://dv-g3c-emudhra.we-have-a-problem.com/",
    "https://ov-g3b-emudhra.we-have-a-problem.com/",
    "https://ov-g3c-emudhra.we-have-a-problem.com/",
    "https://ev-g3b-emudhra.we-have-a-problem.com/",
    "https://ev-g3c-emudhra.we-have-a-problem.com/",
    "https://dv-g3b-aaa-emudhra.we-have-a-problem.com/",
    "https://dv-g3c-aaa-emudhra.we-have-a-problem.com/",
    "https://ov-g3b-aaa-emudhra.we-have-a-problem.com/",
    "https://ov-g3c-aaa-emudhra.we-have-a-problem.com/",
    "https://ev-g3b-aaa-emudhra.we-have-a-problem.com/",
    "https://ev-g3c-aaa-emudhra.we-have-a-problem.com/",
  )

  @Test
  def testCompromisedCA(): Unit =
    assertTrue(s"compromised CAs are in use, or we encountered a network error.\n${testUrls.flatMap(verifyTls).mkString("\n")}",
      Nil == testUrls.flatMap(verifyTls))

  def verifyTls(url: String): Option[String] =
    val conn = URI.create(url).toURL.openConnection().asInstanceOf[HttpsURLConnection]
    conn.setConnectTimeout(10000)
    conn.setReadTimeout(15000)
    try
      val status = conn.getResponseCode
      // Handshake succeeded -> certificate chain was accepted
      Some(s"${red("VULNERABLE")} (HTTP $status) $url")
    catch
      case e: IOException =>
        findSslException(e) match
          case Some(ssl) =>
            None
          case None =>
            // DNS, timeout, connection refused etc. -> says nothing about TLS
            Some(s"${yellow("ERROR")} $url")
    finally conn.disconnect()

  def red(s: String): String =
    Console.RED + s + Console.RESET

  def yellow(s: String): String =
    Console.YELLOW + s + Console.RESET

  // The SSL exception may be wrapped, so walk the cause chain
  def findSslException(t: Throwable): Option[SSLException] =
    Iterator
      .iterate(t)(_.getCause)
      .takeWhile(_ != null)
      .collectFirst { case ssl: SSLException => ssl }

end CompromisedCATest
