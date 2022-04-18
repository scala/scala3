package dotty.tools.scaladoc
package translators

object FilterAttributes:
  def attributesFor(m: Member): Map[String, String] =
    val base = visibity(m) ++ visibity(m) ++ origin(m) ++ keywords(m) ++ inheritedFrom(m)
    base.filter(_._2.nonEmpty)

  private def keywords(m: Member): Map[String, String] =
    Map("keywords" -> m.modifiers.map(_.name).mkString(","))


  private def visibity(m: Member): Map[String, String] =
    Map("visibility" -> m.visibility.name)

  private def inheritedFrom(m: Member): Map[String, String] = m.inheritedFrom match
    case Some(InheritedFrom(name, _, _)) => Map("inherited" -> name)
    case _ => Map.empty

  private def origin(m: Member): Map[String, String] = m.origin match
    case Origin.ImplicitlyAddedBy(name, _) => Map("implicitly" -> s"by $name")
    case Origin.ExtensionFrom(name, _) => Map("extension" -> s"from $name")
    case Origin.ExportedFrom(Some(link)) => Map("export" -> s"from ${link.name}}")
    case _ => Map.empty


  def defaultValues = Map(
    "inherited" ->  "Not inherited",
    "implicitly" -> "Explicit method",
    "extension" -> "Standard member",
    "keywords" -> "no keywords",
    "visibility" -> "public",
  )
