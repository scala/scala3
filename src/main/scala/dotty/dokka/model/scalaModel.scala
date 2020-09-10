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
  case Open extends ScalaOnlyModifiers("open", true)
    
enum VisibilityScope:
  case ImplicitTypeScope // private/protected inside a class or a trait
  case ImplicitModuleScope // private/protected inside a package or an object
  case ExplicitTypeScope(typeName: String) // private[X]/protected[X] inside a class or a trait
  case ExplicitModuleScope(moduleName: String) // private[X]/protected[X] inside a package or an object
  case ThisScope // private[this]/protected[this]

enum ScalaVisibility(val name: String) extends org.jetbrains.dokka.model.Visibility(name, null):
  case Unrestricted extends ScalaVisibility("")
  case Protected(scope: VisibilityScope) extends ScalaVisibility("protected")
  case Private(scope: VisibilityScope) extends ScalaVisibility("private")

enum ScalaModifier(val name: String) extends org.jetbrains.dokka.model.Modifier(name, null):
  case Abstract extends ScalaModifier("abstract")
  case Final extends ScalaModifier("final")
  case Empty extends ScalaModifier("")
