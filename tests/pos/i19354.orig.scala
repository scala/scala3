import javax.annotation.processing.{ AbstractProcessor, RoundEnvironment }
import javax.lang.model.element.{ ElementKind, PackageElement, TypeElement }

import java.util as ju

class P extends AbstractProcessor {
  override def process(annotations: ju.Set[? <: TypeElement], roundEnv: RoundEnvironment): Boolean = {
    annotations
      .stream()
      .flatMap(annotation => roundEnv.getElementsAnnotatedWith(annotation).stream())
      .filter(element => element.getKind == ElementKind.PACKAGE)
      .map(element => element.asInstanceOf[PackageElement])
      .toList()
    true
  }
}
