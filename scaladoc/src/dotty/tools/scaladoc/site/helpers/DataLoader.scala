package dotty.tools.scaladoc.site.helpers

import java.io.File
import org.yaml.snakeyaml.Yaml
import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.collection.mutable

class DataLoader {

  def loadDataDirectory(directoryPath: String): mutable.LinkedHashMap[String, Any] = {
    val dataMap = mutable.LinkedHashMap[String, Any]()
    try {
      loadDirectory(new File(directoryPath), dataMap)
    } catch {
      case e: Exception =>
        println(s"Error loading data directory: ${e.getMessage}")
    }
    dataMap
  }

  private def loadDirectory(directory: File, dataMap: mutable.LinkedHashMap[String, Any]): Unit = {
    if (directory.exists() && directory.isDirectory) {
      for (file <- directory.listFiles()) {
        if (file.isDirectory) {
          loadDirectory(file, dataMap)
        } else {
          readYamlFile(file, dataMap)
        }
      }
    } else {
      println(s"Directory does not exist or is not a directory: ${directory.getPath}")
    }
  }

  private def readYamlFile(file: File, dataMap: mutable.LinkedHashMap[String, Any]): Unit = {
    try {
      val yaml = new Yaml()
      val yamlContents: Any = yaml.load(Source.fromFile(file).mkString)
      val fileName = file.getName.stripSuffix(".yaml").stripSuffix(".yml")

      val parsedContents = yamlContents match {
        case map: java.util.Map[_, _] =>
          println(s"Parsed ${file.getName} as Map")
          map.asScala.toMap
        case list: java.util.List[_] =>
          println(s"Parsed ${file.getName} as List")
          list.asScala.toList
        case other =>
          println(s"Parsed ${file.getName} as ${other.getClass}")
          other
      }

      dataMap.put(fileName, parsedContents)
    } catch {
      case e: Exception =>
        println(s"Error reading YAML file '${file.getName}': ${e.getMessage}")
    }
  }
}
