package dotty.tools
package repl

import java.io.ByteArrayOutputStream
import java.net.InetSocketAddress
import java.util.UUID
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.jar.JarOutputStream

import scala.jdk.CollectionConverters.*

import com.sun.net.httpserver.HttpServer
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test

class DependencyResolverTests:

  /** Serves a single dependency, recording the user agents of the requests */
  private def withRepository[T](organization: String, artifact: String, version: String)
      (body: (String, () => Seq[String]) => T): T =
    val userAgents = ConcurrentLinkedQueue[String]()
    val directory = s"/${organization.replace('.', '/')}/$artifact/$version"
    val pom =
      s"""<project>
         |  <modelVersion>4.0.0</modelVersion>
         |  <groupId>$organization</groupId>
         |  <artifactId>$artifact</artifactId>
         |  <version>$version</version>
         |</project>""".stripMargin.getBytes("UTF-8")
    val jar =
      val out = ByteArrayOutputStream()
      JarOutputStream(out).close()
      out.toByteArray
    val server = HttpServer.create(InetSocketAddress("127.0.0.1", 0), 0)
    server.createContext("/", exchange =>
      userAgents.add(exchange.getRequestHeaders.getFirst("User-Agent"))
      val content = exchange.getRequestURI.getPath match
        case p if p == s"$directory/$artifact-$version.pom" => Some(pom)
        case p if p == s"$directory/$artifact-$version.jar" => Some(jar)
        case _ => None
      content match
        case Some(bytes) =>
          exchange.sendResponseHeaders(200, bytes.length)
          exchange.getResponseBody.write(bytes)
        case None =>
          exchange.sendResponseHeaders(404, -1)
      exchange.close()
    )
    server.start()
    try body(s"http://127.0.0.1:${server.getAddress.getPort}", () => userAgents.asScala.toSeq)
    finally server.stop(0)

  @Test def `dependencies are fetched with the user agent of the REPL`: Unit =
    val property = "coursier.http.agent"
    val explicitUserAgent = sys.props.get(property)
    // a unique version, so that the dependency is not in the coursier cache
    val (organization, artifact, version) = ("dotty.repl.test", "user-agent", UUID.randomUUID().toString)
    withRepository(organization, artifact, version): (url, userAgents) =>
      val repository = DependencyResolver.parseRepository(url).get
      val result = DependencyResolver.resolveDependencies(List((organization, artifact, version)), List(repository))
      assertTrue(result.toString, result.isRight)
      assertFalse(userAgents().isEmpty)
      for userAgent <- userAgents() do
        explicitUserAgent match
          case Some(expected) => assertEquals(expected, userAgent) // e.g. set on the CI
          case None => assertTrue(userAgent, userAgent.startsWith("Scala-REPL/"))
    assertEquals("the property should be restored", explicitUserAgent, sys.props.get(property))
