
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
    // Load the config using ConfigLoader
    val configLoader = new ConfigLoader()
    val basePath = LanguagePickerTag.getConfigFolder
    val config = configLoader.loadConfig(basePath)

    // Accessing languages from the config
    val languagesOpt = config.get[java.util.List[java.util.Map[String, String]]]("languages")

    val languages = languagesOpt match {
      case Some(list) => list.asScala.toList.map(_.asScala.toMap)
      case None => throw new IllegalArgumentException("No languages found in configuration")
    }

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
