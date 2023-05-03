package dotty.tools.scaladoc
package site

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import com.fasterxml.jackson.databind.DeserializationFeature
import java.io.File
import scala.beans._

case class BlogConfig(
  @BeanProperty var input: String,
  @BeanProperty var output: String,
  @BooleanBeanProperty var hidden: Boolean
):
    def this() = this(null, null, false)

object BlogParser:
  def readYml(root: File): BlogConfig =
    val ymlFile = root.toPath
    .resolve("blog.yml")
    .toFile

    if ymlFile.exists then
      val mapper = new ObjectMapper(new YAMLFactory())
      mapper.findAndRegisterModules();

      val blogConfig: BlogConfig = mapper.readValue(ymlFile, classOf[BlogConfig])
      blogConfig
    else new BlogConfig
