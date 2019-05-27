package dotty.tastydoc

import representations._
import references._
import comment.Comment

import scala.annotation.tailrec

import java.io._

class DocPrinter(mutablePackagesMap: scala.collection.mutable.HashMap[String, EmulatedPackageRepresentation], userDocSyntax: String, packagesToLink: List[String]){

  val packagesMap = mutablePackagesMap.toMap

  private def htmlPreCode(content: String, language: String = ""): String = {
    "<pre><code" + (if(language != "") " class=\"language-" + language + "\" " else "") + ">" + content + "</pre></code>"
  }

  private def makeLink(label: String, link: String, hasOwnFile: Boolean, declarationPath: List[String], differentLabelName: Option[String] = None): String = {
    val labelName = differentLabelName match {
      case Some(s) => s
      case None => label
    }

    val packageFormLink = link.replaceFirst("/", "").replaceAll("/", ".")

    if(packagesToLink.exists(packageFormLink.matches(_))){
      @tailrec
      def ascendPath(path: List[String], link: List[String]): String = path match {
        case x::xs if link.nonEmpty && link.head == x => ascendPath(xs, link.tail)
        case _ => (if(path.isEmpty) "." else path.map(_ => "..").mkString("/")) + (if(link.isEmpty) "" else link.mkString("/", "/", ""))
      }

      val relativeLink = {
        if(link == ""){
          if(hasOwnFile){
            if(declarationPath.isEmpty){
              "."
            }else {
              declarationPath.map(_ => "..").mkString("/")
            }
          }else{
            ""
          }
        }else{
          ascendPath(declarationPath, link.split("/").toList.tail)
        }
      }

      if(hasOwnFile){
        "<a href=\"" + relativeLink + "/" + label + ".md\">" + labelName + "</a>"
      } else if(relativeLink == "") {
        labelName
      } else if(relativeLink == "."){
        "<a href=\"#" + label + "\">" + labelName + "</a>"
      } else{
        "<a href=\"" + relativeLink + ".md#" + label + "\">" + labelName + "</a>"
      }
    }else{
      labelName
    }
  }

  private def formatReferences(reference: Reference, declarationPath: List[String]) : String = reference match {
    case CompanionReference(label, link, kind) =>
      makeLink(label, link, true, declarationPath, if(kind.contains("object")) Some(label.stripSuffix("$")) else None)
    case TypeReference(label, link, typeParams, hasOwnFile) =>
      if(typeParams.isEmpty){
        makeLink(label, link, hasOwnFile, declarationPath)
      }else{
        makeLink(label, link, hasOwnFile, declarationPath) + typeParams.map(formatReferences(_, declarationPath)).mkString("[", ", ", "]")
      }
    case OrTypeReference(left, right) =>
      formatReferences(left, declarationPath) + " | " + formatReferences(right, declarationPath)
    case AndTypeReference(left, right) =>
      formatReferences(left, declarationPath) + " & " + formatReferences(right, declarationPath)
    case FunctionReference(args, returnValue, isImplicit) =>
      args.map(formatReferences(_, declarationPath)).mkString("(", ", ", ") => ") + formatReferences(returnValue, declarationPath)
    case TupleReference(args) =>
      args.map(formatReferences(_, declarationPath)).mkString("(", ", ", ")")
    case BoundsReference(low, high) =>
      formatReferences(low, declarationPath) + " <: " + formatReferences(high, declarationPath)
    case ByNameReference(ref) =>
      "=> " + formatReferences(ref, declarationPath)
    case ConstantReference(label) => label
    case NamedReference(name, ref, isRepeated) => name + ": " + formatReferences(ref, declarationPath) + (if(isRepeated) "*" else "")
    case EmptyReference => ""
  }

  private def formatParamList(paramList: ParamList, declarationPath: List[String]) : String = paramList.list.map(x => formatReferences(x, declarationPath)).mkString(
    "(" + (if(paramList.isImplicit) "implicit " else ""),
    ", ",
    ")"
  )

  private def formatModifiers(modifiers: List[String], privateWithin: Option[Reference], protectedWithin: Option[Reference], declarationPath: List[String]): String = {
    val filteredModifiers = modifiers.filter(x => x != "private" && x != "protected")

    (privateWithin match {
      case Some(r) => formatReferences(r, declarationPath).mkString("private[", "", "] ")
      case None if modifiers.contains("private") => "private "
      case None => ""
    }) +
    (protectedWithin match {
      case Some(r) => formatReferences(r, declarationPath).mkString("private[", "", "] ")
      case None if modifiers.contains("protected") => "protected "
      case None => ""
    }) +
    (if(filteredModifiers.nonEmpty) filteredModifiers.mkString("", " ", " ") else "")
  }

  private def formatComments(comment: (Map[String, EmulatedPackageRepresentation], String) => Option[Comment]) : String = comment(packagesMap, userDocSyntax) match {
    case Some(c) =>
      c.body +
      (if(c.authors.nonEmpty) Md.bold(Md.italics("authors")) + " " + c.authors.mkString(", ") else "") +
      (if(c.see.nonEmpty) Md.bold(Md.italics("see")) + " "  + c.see.mkString(", ")else "") +
      (if(c.result.isDefined) Md.bold(Md.italics("return")) + " "  + c.result.get else "") +
      (if(c.throws.nonEmpty) c.throws.map((x, y) => Md.bold(Md.italics(x)) + " " + y).mkString("") else "") +
      (if(c.valueParams.nonEmpty) c.valueParams.map((x, y) => Md.bold(Md.italics(x)) + " " + y).mkString("") else "") +
      (if(c.typeParams.nonEmpty) c.typeParams.map((x, y) => Md.bold(Md.italics(x)) + " " + y).mkString("") else "") +
      (if(c.version.isDefined) Md.bold(Md.italics("version")) + " "  + c.version.get else "") +
      (if(c.since.isDefined) Md.bold(Md.italics("since")) + " "  + c.since.get else "") +
      (if(c.todo.nonEmpty) Md.bold(Md.italics("TODO")) + " " + c.todo.mkString(", ") else "") +
      (if(c.deprecated.isDefined) Md.bold(Md.italics("deprecated")) + " "  + c.deprecated.get else "") +
      (if(c.note.nonEmpty) Md.bold(Md.italics("Note")) + " " + c.note.mkString("") else "") +
      (if(c.example.nonEmpty) Md.bold(Md.italics("Example")) + " " + c.example.mkString("") else "") +
      (if(c.constructor.isDefined) Md.bold(Md.italics("Constructor")) + " "  + c.constructor.get else "") +
      (if(c.group.isDefined) Md.bold(Md.italics("Group")) + " "  + c.group.get else "") +
      (if(c.groupDesc.nonEmpty) c.groupDesc.map((x, y) => Md.bold(Md.italics(x)) + " " + y).mkString("") else "") +
      (if(c.groupNames.nonEmpty) c.groupNames.map((x, y) => Md.bold(Md.italics(x)) + " " + y).mkString("") else "") +
      (if(c.groupPrio.nonEmpty) c.groupPrio.map((x, y) => Md.bold(Md.italics(x)) + " " + y).mkString("") else "") +
      (if(c.hideImplicitConversions.nonEmpty) Md.bold(Md.italics("Hide Implicit Conversions")) + " " + c.hideImplicitConversions.mkString(", ") else "")
    case None => ""
  }

  private def formatAnnotations(annotations: List[TypeReference], declarationPath: List[String]): String = {
    val str = annotations.map("@" + formatReferences(_, declarationPath)).mkString(" ")
    if(str.isEmpty) str else str + " "
  }

  private def formatSimplifiedClassRepresentation(representation: ClassRepresentation, declarationPath: List[String]): String = {
    def formatSimplifiedSignature(): String = {
      htmlPreCode(
        formatAnnotations(representation.annotations, declarationPath) +
        formatModifiers(representation.modifiers, representation.privateWithin, representation.protectedWithin, declarationPath) +
        representation.kind +
        " " +
        makeLink(representation.name, representation.path.mkString("/", "/", ""), true, declarationPath)
      , "scala") +
      "\n"
    }

    formatSimplifiedSignature() +
    formatComments(representation.comments)
  }

  private def formatClassRepresentation(representation: ClassRepresentation, declarationPath: List[String]): String = {
    def formatCompanion(): String = representation.companion match {
      case Some(ref@CompanionReference(_, _, kind)) =>
        Md.header2("Companion " +
          kind +
          " " +
          formatReferences(ref, declarationPath)
          ) +
          "\n"
      case _ => ""
    }

    def formatSignature(): String = {
      htmlPreCode(formatModifiers(representation.modifiers, representation.privateWithin, representation.protectedWithin, representation.path) +
        representation.kind +
        " " +
        representation.name +
        (if(representation.typeParams.nonEmpty) representation.typeParams.mkString("[", ", ", "]") else "") +
        (if(representation.parents.nonEmpty) " extends " + formatReferences(representation.parents.head, representation.path) + representation.parents.tail.map(" with " + formatReferences(_, representation.path)).mkString("") else "")
        , "scala") +
        "\n"
    }

    def formatClassAnnotations(): String = {
      if(representation.annotations.isEmpty){
        ""
      }else{
        Md.header2("Annotations:") +
        formatAnnotations(representation.annotations, declarationPath) +
        "\n"
      }
    }

    def formatKnownSubclasses(): String = {
      if(representation.knownSubclasses.isEmpty){
        ""
      }else{
        Md.header2("Known subclasses:") +
        representation.knownSubclasses.map(formatReferences(_, declarationPath)).mkString(", ") +
        "\n"
      }
    }

    def formatConstructors(): String = {
      if(representation.constructors.isEmpty || representation.isObject){
        ""
      }else{
        Md.header2("Constructors:") +
        representation.constructors.map(ls => htmlPreCode(representation.name + ls.paramLists.map(formatParamList(_, representation.path)).mkString(""), "scala") + "\n" + formatComments(ls.comments)).mkString("") +
        "\n"
      }
    }

    def formatMembers(): String = {

      val typeMembers = representation.members.flatMap {
        case r: TypeRepresentation => Some(r)
        case _ => None
      }
      val objectMembers = representation.members.flatMap {
        case r: ClassRepresentation if r.isObject => Some(r)
        case _ => None
      }
      val classMembers = representation.members.flatMap {
        case r:  ClassRepresentation if !r.isObject && !r.isTrait => Some(r)
        case _ => None
      }
      val traitMembers = representation.members.flatMap {
        case r: ClassRepresentation if r.isTrait => Some(r)
        case _ => None
      }
      val defMembers = representation.members.flatMap {
        case r: DefRepresentation => Some(r)
        case _ => None
      }
      val valMembers = representation.members.flatMap {
        case r: ValRepresentation => Some(r)
        case _ => None
      }

      val abstractTypeMembers =
        typeMembers.filter(_.isAbstract).map(x => Md.header3(x.name) + formatRepresentationToMarkdown(x, declarationPath)).mkString("") +
        objectMembers.filter(_.isAbstract).map{x =>
          traverseRepresentation(x)

          Md.header3(x.name) +
          formatSimplifiedClassRepresentation(x, declarationPath)
        }.mkString("") +
        classMembers.filter(_.isAbstract).map{x =>
          traverseRepresentation(x)

          Md.header3(x.name) +
          formatSimplifiedClassRepresentation(x, declarationPath)
        }.mkString("") +
        traitMembers.filter(_.isAbstract).map{x =>
          traverseRepresentation(x)

          Md.header3(x.name) +
          formatSimplifiedClassRepresentation(x, declarationPath)
        }.mkString("")

      val concreteTypeMembers =
        typeMembers.filter(!_.isAbstract).map(x => Md.header3(x.name) + formatRepresentationToMarkdown(x, declarationPath)).mkString("") +
        objectMembers.filter(!_.isAbstract).map{x =>
          traverseRepresentation(x)

          Md.header3(x.name) +
          formatSimplifiedClassRepresentation(x, declarationPath)
        }.mkString("") +
        classMembers.filter(!_.isAbstract).map{x =>
          traverseRepresentation(x)

          Md.header3(x.name) +
          formatSimplifiedClassRepresentation(x, declarationPath)
        }.mkString("") +
        traitMembers.filter(!_.isAbstract).map{x =>
          traverseRepresentation(x)

          Md.header3(x.name) +
          formatSimplifiedClassRepresentation(x, declarationPath)
        }.mkString("")

      val abstractValueMembers =
        defMembers.filter(_.isAbstract).map(x => Md.header3(x.name) + formatRepresentationToMarkdown(x, declarationPath)).mkString("") +
        valMembers.filter(_.isAbstract).map(x => Md.header3(x.name) + formatRepresentationToMarkdown(x, declarationPath)).mkString("")
      val concreteValueMembers =
        defMembers.filter(!_.isAbstract).map(x => Md.header3(x.name) + formatRepresentationToMarkdown(x, declarationPath)).mkString("") +
        valMembers.filter(!_.isAbstract).map(x => Md.header3(x.name) + formatRepresentationToMarkdown(x, declarationPath)).mkString("")

      (if(abstractTypeMembers.nonEmpty){
        Md.header2("Abstract Type Members:") +
        abstractTypeMembers
      }else{
        ""
      }) +
      (if(concreteTypeMembers.nonEmpty){
        Md.header2("Concrete Type Members:") +
        concreteTypeMembers
      }else{
        ""
      }) +
      (if(abstractValueMembers.nonEmpty){
        Md.header2("Abstract Value Members:") +
        abstractValueMembers
      }else{
        ""
      }) +
      (if(concreteValueMembers.nonEmpty){
        Md.header2("Concrete Value Members:") +
        concreteValueMembers
      }else{
        ""
      })
    }

    Md.header1(representation.kind + " " + representation.name) +
    "\n" +
    formatCompanion() +
    formatSignature() +
    formatComments(representation.comments) +
    formatClassAnnotations() +
    formatKnownSubclasses() +
    formatConstructors() +
    formatMembers()
  }

  private def formatDefRepresentation(representation: DefRepresentation, declarationPath: List[String]): String = {
    htmlPreCode(
      formatAnnotations(representation.annotations, declarationPath) +
      formatModifiers(representation.modifiers, representation.privateWithin, representation.protectedWithin, declarationPath) +
      "def " +
      representation.name +
      (if(representation.typeParams.nonEmpty) representation.typeParams.mkString("[", ", ", "]") else "") +
      representation.paramLists.map(formatParamList(_, declarationPath)).mkString("") +
      ": " +
      formatReferences(representation.returnValue, declarationPath)
    , "scala") +
    "\n" +
    {
      val com = formatComments(representation.comments)
      if(com == "") "\n" else com
    }
  }

  private def formatValRepresentation(representation: ValRepresentation, declarationPath: List[String]): String = {
    htmlPreCode(
      formatAnnotations(representation.annotations, declarationPath) +
      formatModifiers(representation.modifiers, representation.privateWithin, representation.protectedWithin, declarationPath) +
      "val " +
      representation.name +
      ": " +
      formatReferences(representation.returnValue, declarationPath)
    , "scala") +
    "\n" +
    formatComments(representation.comments) +
    "\n"
  }

  private def formatTypeRepresentation(representation: TypeRepresentation, declarationPath: List[String]): String = {
    htmlPreCode(
      formatAnnotations(representation.annotations, declarationPath) +
      formatModifiers(representation.modifiers, representation.privateWithin, representation.protectedWithin, declarationPath) +
      "type " +
      representation.name +
      (if(representation.isAbstract) "" else ": " + formatReferences(representation.alias.get, declarationPath))
    , "scala") +
    "\n" +
    formatComments(representation.comments) +
    "\n"
  }

  def formatRepresentationToMarkdown(representation: Representation, declarationPath: List[String], useSimplifiedFormat: Boolean = false): String = representation match {
    case r: EmulatedPackageRepresentation =>
      if(useSimplifiedFormat){
        htmlPreCode("package " + makeLink(r.name, (r.path :+ r.name).mkString("/", "/", ""), true, declarationPath), "scala") //Package file are at one level below the current package
      }else{
        Md.header1("Package " + r.name) +
        Md.header2("Members:") +
        r.members.foldLeft("")((acc, m) => acc + formatRepresentationToMarkdown(m, declarationPath, true))
      }

    case r : PackageRepresentation =>
      htmlPreCode("package " + makeLink(r.name, (r.path :+ r.name).mkString("/", "/", ""), true, declarationPath), "scala") //Package file are at one level below the current package

    case r: ImportRepresentation => ""

    case r: ClassRepresentation =>
      if(useSimplifiedFormat){
        formatSimplifiedClassRepresentation(r, declarationPath)
      }else{
        formatClassRepresentation(r, declarationPath)
      }

    case r: DefRepresentation => formatDefRepresentation(r, declarationPath)

    case r: ValRepresentation => formatValRepresentation(r, declarationPath)

    case r: TypeRepresentation => formatTypeRepresentation(r, declarationPath)
  }

  //TODO: Remove zzz
  val folderPrefix = "tastydoc/zzz/"
  //TOASK: Path in representation with or without name at the end?
  def traverseRepresentation(representation: Representation): Unit = representation match {
    case r: EmulatedPackageRepresentation =>
      r.members.foreach(traverseRepresentation)
      val file = new File("./" + folderPrefix + (r.path :+ r.name).mkString("/", "/", "/") + r.name + ".md")
      file.getParentFile.mkdirs
      val pw = new PrintWriter(file)
      pw.write(formatRepresentationToMarkdown(r, (r.path :+ r.name)))
      pw.close

    case r: PackageRepresentation =>
      r.members.foreach(traverseRepresentation)

    case r: ClassRepresentation =>
      val filename = if(r.isObject) r.name + "$" else r.name
      val file = new File("./" + folderPrefix + r.path.mkString("", "/", "/") + filename + ".md")
      file.getParentFile.mkdirs
      val pw = new PrintWriter(file)
      pw.write(formatRepresentationToMarkdown(r, r.path))
      pw.close

    case _ =>
  }
}