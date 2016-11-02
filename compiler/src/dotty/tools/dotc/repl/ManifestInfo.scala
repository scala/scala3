package dotty.tools.dotc.repl

import java.net.JarURLConnection
import scala.collection.JavaConversions._

object ManifestInfo {

  val attributes: Map[String, String] = {
    for {
      resourceUrl <- Option(getClass.getResource(getClass.getSimpleName + ".class"))
      urlConnection = resourceUrl.openConnection() if urlConnection.isInstanceOf[JarURLConnection]
      manifest <- Option(urlConnection.asInstanceOf[JarURLConnection].getManifest)
    } yield {
      manifest.getMainAttributes.foldLeft(Map[String, String]())(
        (map, attribute) => map + (attribute._1.toString -> attribute._2.toString)
      )
    }
  }.getOrElse(Map())

}
