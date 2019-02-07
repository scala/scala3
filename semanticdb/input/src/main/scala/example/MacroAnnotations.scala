/*
// This text is deactivated for now as macro annotations require to enable
// macro paradise

package example


import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly
import scala.language.experimental.macros

@compileTimeOnly("enable macro paradise to expand macro annotations")
class MacroAnnotation extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = ???
}

@MacroAnnotation
class MacroAnnotations
object MacroAnnotations
*/