package dotty.tools.scaladoc.site

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import com.fasterxml.jackson.databind.DeserializationFeature
import java.io.File
import scala.beans.{BooleanBeanProperty, BeanProperty}
import scala.util.Try

case class BlogConfig(
  @BeanProperty input: String | Null,
  @BeanProperty output: String | Null,
  @BooleanBeanProperty hidden: Boolean
):
  def this() = this(null, null, false)

object BlogParser:
  def readYml(content: File | String): BlogConfig =
    val mapper = ObjectMapper(YAMLFactory())
      .findAndRegisterModules()

    content match
      case f: File =>
        val ymlFile = f.toPath.resolve("blog.yml").toFile
        if ymlFile.exists then mapper.readValue(ymlFile, classOf[BlogConfig]) else new BlogConfig
      case s: String => Try(mapper.readValue(s, classOf[BlogConfig])).getOrElse(new BlogConfig)
