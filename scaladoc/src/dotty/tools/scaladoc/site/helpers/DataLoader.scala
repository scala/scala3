package dotty.tools.scaladoc.site.helpers

import java.io.File
import org.yaml.snakeyaml.Yaml
import scala.io.Source
import scala.jdk.CollectionConverters._
import java.util.{LinkedHashMap, List, Map}

class DataLoader {

  def loadDataDirectory(directoryPath: String): LinkedHashMap[String, Object] = {
    val dataMap = new LinkedHashMap[String, Object]()
    try {
      loadDirectory(new File(directoryPath), dataMap)
    } catch {
      case e: Exception =>
        println(s"Error loading data directory: ${e.getMessage}")
    }
    dataMap
  }

  private def loadDirectory(directory: File, dataMap: LinkedHashMap[String, Object]): Unit = {
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

  private def readYamlFile(file: File, dataMap: LinkedHashMap[String, Object]): Unit = {
    try {
      val yaml = new Yaml()
      val yamlContents: Object = convertToJava(yaml.load(Source.fromFile(file).mkString))
      val fileName = file.getName.stripSuffix(".yaml").stripSuffix(".yml")

      dataMap.put(fileName, yamlContents)
    } catch {
      case e: Exception =>
        println(s"Error reading YAML file '${file.getName}': ${e.getMessage}")
    }
  }

  private def convertToJava(value: Any): Object = {
    value match {
      case map: java.util.Map[_, _] =>
        val javaMap = new LinkedHashMap[String, Object]()
        map.asScala.foreach { case (k, v) => javaMap.put(k.toString, convertToJava(v)) }
        javaMap.asInstanceOf[Object]
      case list: java.util.List[_] =>
        val javaList = new java.util.ArrayList[Object]()
        list.asScala.foreach(v => javaList.add(convertToJava(v)))
        javaList.asInstanceOf[Object]
      case other =>
        other.asInstanceOf[Object]
    }
  }
}
