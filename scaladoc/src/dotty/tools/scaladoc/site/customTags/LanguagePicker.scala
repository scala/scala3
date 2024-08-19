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

  override def render(
      context: TemplateContext,
      args: Array[? <: LNode]
  ): Object = {
    if (args.isEmpty || args(0) == null) {
      throw new IllegalArgumentException(
        "The 'languages' argument is required and must be provided in the {% language_picker %} tag."
      )
    }

    // Raw argument value
    val rawArgument =
      args(0).render(context).toString.trim.stripPrefix("languages=")
    println(s"Raw languages argument: $rawArgument")

    // Determine if the argument is a variable reference or a literal string
    val languagesArgString = if (rawArgument.startsWith("page.")) {
      // Extract the variable name and fetch its value from the context
      val variableName = rawArgument.stripPrefix("page.")
      val siteData = context
        .getVariables()
        .get("page")
        .asInstanceOf[java.util.Map[String, Any]]
      val variableValue = siteData.get(variableName) match {
        case value: String => value
        case _ =>
          println(s"Variable '$variableName' not found or not a string")
          ""
      }
      println(s"Resolved variable value: $variableValue")
      variableValue
    } else {
      // Treat the argument as a literal string
      rawArgument
    }

    println(s"Language argument string: $languagesArgString")
    // Extract language codes from the string
    val languagesArg = extractLanguageCodes(languagesArgString)
    println(s"Parsed languages: ${languagesArg.mkString(", ")}")

    // Load the config using ConfigLoader
    val configLoader = new ConfigLoader()
    val basePath = LanguagePickerTag.getConfigFolder
    val config = configLoader.loadConfig(basePath)

    // Access languages from the config
    val languagesOpt =
      config.get[java.util.List[java.util.Map[String, String]]]("languages")
    val languagesFromConfig = languagesOpt match {
      case Some(list) => list.asScala.toList.map(_.asScala.toMap)
      case None =>
        throw new IllegalArgumentException(
          "No languages found in configuration"
        )
    }

    // Extract the list of codes from the config
    val configLanguageCodes = languagesFromConfig.map(_("code"))
    println(s"Languages from config: ${configLanguageCodes.mkString(", ")}")

    // Validate that all languages in languagesArg exist in the config
    val missingLanguages = languagesArg.filterNot(configLanguageCodes.contains)
    if (missingLanguages.nonEmpty) {
      throw new IllegalArgumentException(
        s"The following languages are not defined in the configuration: ${missingLanguages.mkString(", ")}"
      )
    }

    // Filter the languages from the config based on the languagesArg
    val languages = languagesFromConfig.filter(language =>
      languagesArg.contains(language("code"))
    )

    // Create the dropdown HTML
    val dropdown = new StringBuilder
    dropdown.append(
      "<select id='language-selector' name='language' onChange='handleLanguageChange(this)' style='font-size: 14px; padding: 5px; border: 1px solid #ccc; border-radius: 3px;'>"
    )
    for (language <- languages) {
      val code = language("code")
      val name = language("name")
      dropdown.append(s"<option value='$code'>$name</option>")
    }
    dropdown.append("</select>")

    dropdown.toString()
  }

  private def extractLanguageCodes(languagesArg: String): List[String] = {
    // Regular expression to match both two-letter codes and hyphenated parts
    val pattern = """[a-z]{2}(-[a-z]{2})?""".r

    // Find all matches in the string
    val matches = pattern.findAllIn(languagesArg).toList
    println(s"Extracted language codes: ${matches.mkString(", ")}")

    // Ensure that each part is valid and non-empty
    matches.map(_.trim).filter(_.nonEmpty)
  }
}

object LanguagePickerTag {
  @volatile private var configFolder: String = "_docs"

  def setConfigFolder(path: String): Unit = {
    configFolder = path
  }

  def getConfigFolder: String = configFolder
}
