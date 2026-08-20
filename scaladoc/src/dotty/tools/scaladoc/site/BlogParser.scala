package dotty.tools.scaladoc.site

import _root_.tools.jackson.databind.DeserializationFeature
import _root_.tools.jackson.dataformat.yaml.YAMLMapper
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
    val mapper = YAMLMapper.builder()
      .disable(DeserializationFeature.FAIL_ON_NULL_FOR_PRIMITIVES)
      .build()

    content match
      case f: File =>
        val ymlFile = f.toPath.resolve("blog.yml").toFile
        if ymlFile.exists then mapper.readValue(ymlFile, classOf[BlogConfig]) else new BlogConfig
      case s: String => Try(mapper.readValue(s, classOf[BlogConfig])).getOrElse(new BlogConfig)
