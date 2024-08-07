package dotty.tools.scaladoc.site.helpers


import java.io.{File, InputStream}
import java.nio.file.{Files, Paths, Path}
import scala.jdk.CollectionConverters._
import scala.collection.mutable
import scala.language.dynamics
import scala.language.dynamics
import scala.language.dynamics
import liqp.TemplateContext
import liqp.nodes.LNode
import liqp.tags.Tag
import org.yaml.snakeyaml.Yaml


class Config(private val data: mutable.LinkedHashMap[String, Any]) extends Dynamic {
  def selectDynamic(field: String): Any = {
    data.get(field) match {
      case Some(value: mutable.LinkedHashMap[_, _]) =>
        new Config(value.asInstanceOf[mutable.LinkedHashMap[String, Any]])
      case Some(value: java.util.ArrayList[_]) =>
        value.asScala.toSeq.map {
          case map: java.util.Map[_, _] =>
            new Config(map.asInstanceOf[java.util.Map[String, Any]].asScala.to(mutable.LinkedHashMap))
          case other => other
        }
      case Some(value) => value
      case None => throw new NoSuchElementException(s"No such element: $field")
    }
  }

  def get[T](key: String): Option[T] = {
    data.get(key).map(_.asInstanceOf[T])
  }

}

class ConfigLoader {
  def loadConfig(basePath: String): Config = {
    val configMap = mutable.LinkedHashMap[String, Any]()
    val yamlFileNames = Seq("_config.yml", "_config.yaml")

    try {
      val yaml = new Yaml()
      val baseDir = new File(basePath).getAbsolutePath

      val configFile = yamlFileNames
        .map(baseDir + File.separator + _)
        .map(Paths.get(_))
        .find(Files.exists(_))

      configFile match {
        case Some(path) =>
          val inputStream: InputStream = Files.newInputStream(path)
          val data = yaml.load[java.util.Map[String, Any]](inputStream)
          configMap ++= data.asScala.toMap
        case None =>
          println(s"Warning: No config file found in path: $baseDir")
      }
    } catch {
      case e: Exception =>
        println(s"Error loading config file: ${e.getMessage}")
    }

    new Config(configMap)
  }
}
