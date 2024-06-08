package dotty.tools.scaladoc.site.helpers

import java.io.File
import org.yaml.snakeyaml.Yaml
import scala.io.Source
import scala.jdk.CollectionConverters._

/**
 * Helper class to load data from YAML files in a directory.
 */
class DataLoader {
  /**
   * Loads data from YAML files in the specified directory.
   *
   * @param directoryPath The path to the directory containing YAML files.
   * @return A map containing data loaded from YAML files, where the keys are file names (without extension)
   *         and the values are maps representing the contents of each YAML file.
   */
  def loadDataDirectory(directoryPath: String): java.util.Map[String, Any] = {
    val dataMap = new java.util.LinkedHashMap[String, Any]() // Change HashMap to LinkedHashMap
    try {
      loadDirectory(new File(directoryPath), dataMap)
    } catch {
      case e: Exception =>
        println(s"Error loading data directory: ${e.getMessage}")
    }
    dataMap
  }


  /**
   * Recursively loads data from YAML files in the specified directory and its subdirectories.
   *
   * @param directory The directory to search for YAML files.
   * @param dataMap The map to store the loaded data.
   */
  private def loadDirectory(directory: File, dataMap: java.util.LinkedHashMap[String, Any]): Unit = {
    if (directory.exists() && directory.isDirectory) {
      for (file <- directory.listFiles()) {
        if (file.isDirectory) {
          // Recursive call for subdirectories
          loadDirectory(file, dataMap)
        } else {
          // Read and parse YAML file
          val yamlContents = readYamlFile(file)
          if (yamlContents != null) {
            val fileName = file.getName.stripSuffix(".yaml").stripSuffix(".yml")
            dataMap.put(fileName, yamlContents)
          }
        }
      }
    }
  }

  /**
   * Reads and parses a YAML file.
   *
   * @param file The YAML file to read.
   * @return A map representing the contents of the YAML file, or `null` if an error occurs.
   */
  private def readYamlFile(file: File): java.util.Map[String, Any] = {
    try {
      val yaml = new Yaml()
      val yamlContents = yaml.load(Source.fromFile(file).mkString).asInstanceOf[java.util.Map[String, Any]]

      // Convert to Scala mutable map before converting to Java map
      val scalaMap = yamlContents.asScala
      convertToJavaMap(scalaMap)
    } catch {
      case e: Exception =>
        println(s"Error reading YAML file '${file.getName}': ${e.getMessage}")
        null // Handle any exceptions while reading or parsing YAML
    }
  }


  /**
   * Converts a Scala map to a Java map.
   *
   * @param scalaMap The Scala map to convert.
   * @return The equivalent Java map.
   */
  private def convertToJavaMap(scalaMap: scala.collection.mutable.Map[String, Any]): java.util.Map[String, Any] = {
    val javaMap = new java.util.LinkedHashMap[String, Any]()
    scalaMap.foreach { case (key, value) =>
      javaMap.put(key, convertValue(value))
    }
    javaMap
  }


  /**
   * Recursively converts values to appropriate types.
   *
   * @param value The value to convert.
   * @return The converted value.
   */
  private def convertValue(value: Any): Any = value match {
    case map: java.util.Map[_, _] =>
      map.asScala.map { case (k, v) => k.toString -> convertValue(v) }.toMap // Convert to Scala map
    case list: java.util.List[_] =>
      list.asScala.map(convertValue).asJava // Convert to Scala list recursively
    case _ => value
  }

}
