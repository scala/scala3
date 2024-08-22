package dotty.tools.scaladoc.site.tags

import java.nio.file.Paths
import scala.jdk.CollectionConverters._
import liqp.TemplateContext
import liqp.nodes.LNode
import liqp.tags.Tag
import dotty.tools.scaladoc.site.helpers.ConfigLoader
import dotty.tools.scaladoc.site.helpers.Config

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
    val (languagesArg, valueType) = if (rawArgument.startsWith("page.")) {
      // Extract the variable name and fetch its value from the context
      val variableName = rawArgument.stripPrefix("page.")
      val siteData = context
        .getVariables()
        .get("page")
        .asInstanceOf[java.util.Map[String, Any]]
      println(siteData)

      // Handle both String and List cases
      siteData.get(variableName) match {
        case value: String =>
          println(s"Resolved variable value (string): $value")
          (List(value), "String")
        case value: java.util.List[_] =>
          println(
            s"Resolved variable value (list): ${value.asScala.mkString(", ")}"
          )
          (value.asScala.toList.collect { case s: String => s }, "List")
        case value =>
          println(
            s"Variable '$variableName' found but not a valid string/list. Type: ${if (value != null) value.getClass.getSimpleName else "null"}"
          )
          (
            List.empty[String],
            if (value != null) value.getClass.getSimpleName else "null"
          )
      }
    } else {
      // Treat the argument as a literal string and split it by comma if necessary
      (rawArgument.split(",").map(_.trim).toList, "Literal String")
    }

    // Log the type of the resolved argument
    println(s"Resolved argument type: $valueType")

    // Ensure "en" is the first element in the list
    val languagesWithEnFirst = {
      if (languagesArg.contains("en")) {
        // If "en" is already in the list, move it to the first position
        "en" :: languagesArg.filterNot(_ == "en")
      } else {
        // Otherwise, prepend "en" to the list
        "en" :: languagesArg
      }
    }
    println(
      s"Languages after ensuring 'en' is first: ${languagesWithEnFirst.mkString(", ")}"
    )

    // Early return if the languages argument is empty
    if (languagesWithEnFirst.isEmpty) {
      println("Languages argument is empty, not rendering anything.")
      return ""
    }

    println(s"Language argument string: ${languagesWithEnFirst.mkString(", ")}")
    // Extract language codes from the list
    val extractedLanguageCodes =
      languagesWithEnFirst.flatMap(extractLanguageCodes)
    println(s"Parsed languages: ${extractedLanguageCodes.mkString(", ")}")

    // Load the config using ConfigLoader
    val config = LanguagePickerTag.getConfigValue

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

    // Validate that all languages in extractedLanguageCodes exist in the config
    val missingLanguages =
      extractedLanguageCodes.filterNot(configLanguageCodes.contains)
    if (missingLanguages.nonEmpty) {
      throw new IllegalArgumentException(
        s"The following languages are not defined in the configuration: ${missingLanguages.mkString(", ")}"
      )
    }

    // Filter the languages from the config based on the extractedLanguageCodes
    val languages = languagesFromConfig.filter(language =>
      extractedLanguageCodes.contains(language("code"))
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
    dropdown.append(
      "</select> <script>var availableLanguages = " + languagesWithEnFirst
        .map("'" + _ + "'")
        .mkString(
          "[",
          ", ",
          "]"
        ) + "; console.log('Available languages: ', availableLanguages);</script>"
    )

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
  @volatile private var config: Config = null

  def setConfigValue(configuration: Config): Unit = {
    config = configuration
  }

  def getConfigValue: Config = config
}
