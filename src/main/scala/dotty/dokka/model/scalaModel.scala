package dotty.dokka

import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model._
import collection.JavaConverters._
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.doc._
import org.jetbrains.dokka.model.properties._  

case class TastyDocumentableSource(val path: String, val lineNumber: Int) extends DocumentableSource {
    override def getPath = path
}

enum ScalaOnlyModifiers(val name: String, val prefix: Boolean) extends ExtraModifiers(name, null):
  case Sealed extends ScalaOnlyModifiers("sealed", true)
  case Case extends ScalaOnlyModifiers("case", false)
  case Implicit extends ScalaOnlyModifiers("implicit", true)
  case Inline extends ScalaOnlyModifiers("inline", true)
  case Lazy extends ScalaOnlyModifiers("lazy", true)
  case Override extends ScalaOnlyModifiers("override", true)
  case Erased extends ScalaOnlyModifiers("erased", true)
  case Opaque extends ScalaOnlyModifiers("opaque", true)
    
enum ScalaVisibility(val name: String) extends org.jetbrains.dokka.model.Visibility(name, null):
  case NoModifier extends ScalaVisibility("")
  case Protected extends ScalaVisibility("protected")
  case Private extends ScalaVisibility("private")

enum ScalaModifier(val name: String) extends org.jetbrains.dokka.model.Modifier(name, null):
  case Abstract extends ScalaModifier("abstract")
  case Final extends ScalaModifier("final")
  case Empty extends ScalaModifier("")
