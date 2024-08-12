
package dotty.tools.scaladoc.site.tags

import java.nio.file.{Files, Paths, Path}
import java.io.{File, InputStream}
import scala.jdk.CollectionConverters._
import scala.collection.mutable
import scala.language.dynamics
import scala.language.dynamics
import liqp.TemplateContext
import liqp.nodes.LNode
import liqp.tags.Tag
import scala.language.dynamics
import dotty.tools.scaladoc.site.helpers.{ConfigLoader}
import org.yaml.snakeyaml.Yaml


class LanguagePickerTag extends Tag("language_picker") {

  override def render(context: TemplateContext, args: Array[? <: LNode]): Object = {
    // Check if the languages argument is provided
    if (args.isEmpty || args(0) == null) {
      throw new IllegalArgumentException("The 'languages' argument is required and must be provided in the {% language_picker %} tag.")
    }

    // Load the config using ConfigLoader
    val configLoader = new ConfigLoader()
    val basePath = LanguagePickerTag.getConfigFolder
    val config = configLoader.loadConfig(basePath)

    // Extract the languages argument as a string
    val languagesArgString = args(0).render(context).toString.trim

    // Ensure that the argument starts with "languages=" and parse it correctly
    if (!languagesArgString.startsWith("languages=")) {
      throw new IllegalArgumentException("The 'languages' argument is malformed. It should start with 'languages=[' and end with ']'")
    }

    // Parse the string into a list of languages
    val languagesArg = languagesArgString
      .stripPrefix("languages=")
      .stripPrefix("[")
      .stripSuffix("]")
      .split(",")
      .map(_.trim.stripPrefix("'").stripSuffix("'"))
      .toList

    // Debugging output
    println(s"Parsed languages: ${languagesArg.mkString(", ")}")

    // Accessing languages from the config
    val languagesOpt = config.get[java.util.List[java.util.Map[String, String]]]("languages")
    val languagesFromConfig = languagesOpt match {
      case Some(list) => list.asScala.toList.map(_.asScala.toMap)
      case None => throw new IllegalArgumentException("No languages found in configuration")
    }

    // Extract the list of codes from the config
    val configLanguageCodes = languagesFromConfig.map(_("code"))

    // Validate that all languages in languagesArg exist in the config
    val missingLanguages = languagesArg.filterNot(configLanguageCodes.contains)
    if (missingLanguages.nonEmpty) {
      throw new IllegalArgumentException(s"The following languages are not defined in the configuration: ${missingLanguages.mkString(", ")}")
    }

    // Filter the languages from the config based on the languagesArg
    val languages = languagesFromConfig.filter(language => languagesArg.contains(language("code")))

    // Create the dropdown HTML
    val dropdown = new StringBuilder
    dropdown.append("<select name='language' style='font-size: 14px; padding: 5px; border: 1px solid #ccc; border-radius: 3px;'>")
    for (language <- languages) {
      val code = language("code")
      val name = language("name")
      dropdown.append(s"<option value='$code'>$name</option>")
    }

    dropdown.append("</select>")
    dropdown.toString()
  }
}



object LanguagePickerTag {
  @volatile private var configFolder: String = "_docs"

  def setConfigFolder(path: String): Unit = {
    configFolder = path
  }

  def getConfigFolder: String = configFolder
}
