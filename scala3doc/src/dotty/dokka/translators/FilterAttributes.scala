package dotty.dokka.translators

import org.jetbrains.dokka.base.translators.documentables.{DefaultPageCreator, PageContentBuilder, PageContentBuilder$DocumentableContentBuilder}
import org.jetbrains.dokka.base.signatures.SignatureProvider
import org.jetbrains.dokka.base.transformers.pages.comments.CommentsToContentConverter
import org.jetbrains.dokka.transformers.documentation.DocumentableToPageTranslator
import org.jetbrains.dokka.utilities.DokkaLogger
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.pages._
import collection.JavaConverters._
import org.jetbrains.dokka.model.properties._
import org.jetbrains.dokka.base.transformers.documentables.CallableExtensions
import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet
import org.jetbrains.dokka.base.resolvers.anchors._
import org.jetbrains.dokka.model.properties.PropertyContainer
import org.jetbrains.dokka.model.doc._
import dotty.dokka.model.api._

import dotty.dokka._

object FilterAttributes:
  def attributesFor(m: Member): Map[String, String] =
    val base = visibity(m) ++ visibity(m) ++ origin(m) ++ keywords(m) ++ inheritedFrom(m)
    base.filter(_._2.nonEmpty)

  private def keywords(m: Member): Map[String, String] = 
    Map("keywords" -> m.modifiers.map(_.name).mkString(","))


  private def visibity(m: Member): Map[String, String] =
    Map("visibility" -> m.visibility.name)

  private def inheritedFrom(m: Member): Map[String, String] = m.inheritedFrom match
    case Some(InheritedFrom(name, _)) => Map("inherited" -> name)
    case _ => Map.empty

  private def origin(m: Member): Map[String, String] = m.origin match
    case Origin.ImplicitlyAddedBy(name, _) => Map("implicitly" -> s"by $name")
    case Origin.ExtensionFrom(name, _) => Map("extension" -> s"from $name")
    case Origin.ExportedFrom(name, _) => Map("export" -> s"from $name")
    case _ => Map.empty


  def defaultValues = Map(
    "inherited" ->  "Not inherited",
    "implicitly" -> "Explicit method",
    "extension" -> "Standard member",
    "keywords" -> "no keywords",
    "visibility" -> "public",
  )
