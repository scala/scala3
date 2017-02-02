package dotty.tools
package dottydoc
package staticsite

import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.core.`type`.TypeReference

object Yaml {
  import scala.collection.JavaConverters._
  import java.util.HashMap
  import java.io.ByteArrayInputStream

  def apply(input: String): HashMap[String, AnyRef] = {
    val is = new ByteArrayInputStream(input.getBytes("UTF-8"))
    val mapper = new ObjectMapper(new YAMLFactory())

    val typeRef: TypeReference[HashMap[String, AnyRef]] =
      new TypeReference[HashMap[String, AnyRef]] {}

    mapper.readValue(is, typeRef)
  }
}
