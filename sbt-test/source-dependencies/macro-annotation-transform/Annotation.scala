package example

import java.nio.file.Files
import scala.quoted.*

final class annot extends annotation.MacroAnnotation {
  def transform(using q: Quotes)(
    definition: q.reflect.Definition,
    companion: Option[q.reflect.Definition],
  ): List[q.reflect.Definition] = {
    Files.writeString(outputFile, "transform1")
    List(definition)
  }
}
