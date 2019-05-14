package dotty.tastydoc

import representations._
import references._
import comment.Comment

import scala.annotation.tailrec

import java.io._

object DocPrinter{

  private def htmlPreCode(content: String, language: String = ""): String = {
    "<pre><code" + (if(language != "") " class=\"language-" + language + "\" " else "") + ">" + content + "</pre></code>"
  }

  private def makeLink(label: String, link: String, hasOwnFile: Boolean, declarationPath: List[String]): String = {
    val packageFormLink = link.replaceAll("\\./", "").replaceAll("/", ".")
    println(link + label)
    if(TastydocConsumer.packagesToLink.exists(packageFormLink.matches(_))){
      @tailrec
      def ascendPath(path: List[String], link: List[String]): String = path match {
        case x::xs if link.nonEmpty && link.head == x => ascendPath(xs, link.tail)
        case _ => (if(path.isEmpty) "." else path.map(_ => "..").mkString("/")) + (if(link.isEmpty) "" else link.mkString("/", "/", ""))
      }

      val relativeLink = {
        if(link == "."){
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
        "<a href=\"" + relativeLink + "/" + label + ".md\">" + label + "</a>"
      } else if(relativeLink == "") {
        label
      } else if(relativeLink == "."){
        "<a href=\"#" + label + "\">" + label + "</a>"
      } else{
        "<a href=\"" + relativeLink + ".md#" + label + "\">" + label + "</a>"
      }
    }else{
      label
    }
  }

  private def formatReferences(reference: Reference, declarationPath: List[String]) : String = reference match {
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
      formatReferences(low, declarationPath) + ">:" + formatReferences(high, declarationPath)
    case ByNameReference(ref) =>
      "=> " + formatReferences(ref, declarationPath)
    case ConstantReference(label) => label
    case NamedReference(name, ref, isRepeated) => name + ": " + formatReferences(ref, declarationPath) + (if(isRepeated) "*" else "")
    case EmptyReference => throw Exception("EmptyReference should never occur outside of conversion from reflect.")
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

  private def formatComments(comment: Option[Comment]) : String = comment match {
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

  private def formatSimplifiedClassRepresentation(representation: ClassRepresentation, declarationPath: List[String]): String = {
    def formatSimplifiedSignature(): String = {
      htmlPreCode(formatModifiers(representation.modifiers, representation.privateWithin, representation.protectedWithin, declarationPath) +
        representation.kind +
        " " +
        makeLink(representation.name, declarationPath.mkString("/", "/", ""), true, declarationPath)
        , "scala") +
        "\n"
    }

    formatSimplifiedSignature() +
    formatComments(representation.comments)
  }

  private def formatClassRepresentation(representation: ClassRepresentation, declarationPath: List[String]): String = {
    def formatCompanion(): String = (representation.companion, representation.companionKind) match {
      case (Some(ref@TypeReference(label, link, ls, hasOwnFile)), Some(kind)) =>
        Md.header2("Companion " +
          kind +
          " " +
          (if(kind.contains("object")) formatReferences(TypeReference(label, link, ls, hasOwnFile), declarationPath) else formatReferences(ref, declarationPath))
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

    def formatAnnotations(): String = {
      if(representation.annotations.isEmpty){
        ""
      }else{
        Md.header2("Annotations:") +
        representation.annotations.mkString("\n") +
        "\n"
      }
    }

    def formatConstructors(): String = {
      if(representation.constructors.isEmpty){
        ""
      }else{
        Md.header2("Constructors:") +
        representation.constructors.map((ls, com) => htmlPreCode(representation.name + ls.paramLists.map(formatParamList(_, representation.path)).mkString(""), "scala") + "\n" + formatComments(com)).mkString("") +
        "\n"
      }
    }

    def formatMembers(): String = {
      Md.header2("Type Members:") +
      representation.members.flatMap{
        case x: TypeRepresentation => Some(x)
        case _ => None
        }
        .map(x => Md.header3(x.name) + formatRepresentationToMarkdown(x, declarationPath)).mkString("") +
      Md.header2("Object members:") +
      representation.members.flatMap{
        case x : ClassRepresentation if !x.isObject => Some(x)
        case _ => None
        }
        .map{x =>
          traverseRepresentation(x, Set.empty)

          Md.header3(x.name) +
          formatSimplifiedClassRepresentation(x, declarationPath :+ representation.name) // Need one more level of declarationPath for linking to itself
        }.mkString("") +
      Md.header2("Class Members:") +
      representation.members.flatMap{
        case x: ClassRepresentation if !x.isTrait && !x.isObject => Some(x)
        case _ => None
        }
        .map{x =>
          traverseRepresentation(x, Set.empty)

          Md.header3(x.name) +
          formatSimplifiedClassRepresentation(x, declarationPath :+ representation.name) // Need one more level of declarationPath for linking to itself
        }.mkString("") +      Md.header2("Trait Members:") +
      representation.members.flatMap{
        case x: ClassRepresentation if x.isTrait => Some(x)
        case _ => None
        }
        .map{x =>
          traverseRepresentation(x, Set.empty)

          Md.header3(x.name) +
          formatSimplifiedClassRepresentation(x, declarationPath :+ representation.name) // Need one more level of declarationPath for linking to itself
        }.mkString("") +
      Md.header2("Definition members:") +
      representation.members.flatMap{
        case x : DefRepresentation => Some(x)
        case _ => None
        }
        .map(x => Md.header3(x.name) + formatRepresentationToMarkdown(x, declarationPath)).mkString("") +
      Md.header2("Value members:") +
      representation.members.flatMap{
        case x : ValRepresentation => Some(x)
        case _ => None
        }
        .map(x => Md.header3(x.name) + formatRepresentationToMarkdown(x, declarationPath)).mkString("")
    }

    Md.header1(representation.kind + " " + representation.name) +
    "\n" +
    formatCompanion() +
    formatSignature() +
    formatComments(representation.comments) +
    formatAnnotations() +
    formatConstructors() +
    formatMembers()
  }

  private def formatDefRepresentation(representation: DefRepresentation, declarationPath: List[String]): String = {
    htmlPreCode(
    formatModifiers(representation.modifiers, representation.privateWithin, representation.protectedWithin, declarationPath) +
    "def " +
    representation.name +
    (if(representation.typeParams.nonEmpty) representation.typeParams.mkString("[", ", ", "]") else "") +
    representation.paramLists.map(formatParamList(_, declarationPath)).mkString("") +
    ": " +
    formatReferences(representation.returnValue, declarationPath), "scala") +
    "\n" +
    {
      val com = formatComments(representation.comments)
      if(com == "") "\n" else com
    }
  }

  private def formatValRepresentation(representation: ValRepresentation, declarationPath: List[String]): String = {
    htmlPreCode(
    formatModifiers(representation.modifiers, representation.privateWithin, representation.protectedWithin, declarationPath) +
    "val " +
    representation.name +
    ": " +
    formatReferences(representation.returnValue, declarationPath), "scala") +
    "\n" +
    formatComments(representation.comments) +
    "\n"
  }

  private def formatTypeRepresentation(representation: TypeRepresentation, declarationPath: List[String]): String = {
    htmlPreCode(
    formatModifiers(representation.modifiers, representation.privateWithin, representation.protectedWithin, declarationPath) +
    "type " +
    representation.name +
    ": ", "scala") +
    "\n" +
    formatComments(representation.comments) +
    "\n"
  }

  def formatRepresentationToMarkdown(representation: Representation, declarationPath: List[String]): String = representation match {
    case r : PackageRepresentation =>
      r.name +
      "\n" +
      r.members.map(formatRepresentationToMarkdown(_, r.path)).mkString("\n")

    case r: ImportRepresentation =>
      "import " +
      r.path.mkString("", ".", ".") +
      r.name +
      "\n"

    case r: ClassRepresentation => formatClassRepresentation(r, declarationPath)

    case r: DefRepresentation => formatDefRepresentation(r, declarationPath)

    case r: ValRepresentation => formatValRepresentation(r, declarationPath)

    case r: TypeRepresentation => formatTypeRepresentation(r, declarationPath)
  }

  //TODO: Remove zzz
  val folderPrefix = "tastydoc/zzz/"
  //TOASK: Path in representation with or without name at the end?
  def traverseRepresentation(representation: Representation, packagesSet: Set[(List[String], String)]) : Set[(List[String], String)] = representation match {
    case r: PackageRepresentation =>
      if(r.path.nonEmpty && r.name != "<empty>"){
        val z = packagesSet + ((r.path, "package " + Md.link(r.name, "./" + r.path.last + "/" + r.name + ".md")))
        r.members.foldLeft(z)((acc, m) => traverseRepresentation(m, acc))
      }else{
        r.members.foldLeft(packagesSet)((acc, m) => traverseRepresentation(m, acc))
      }

    case r: ClassRepresentation =>
      val file = new File("./" + folderPrefix + r.path.mkString("", "/", "/") + r.name + ".md")
      file.getParentFile.mkdirs
      val pw = new PrintWriter(file)
      pw.write(formatRepresentationToMarkdown(r, r.path))
      pw.close

      packagesSet + ((r.path, formatSimplifiedClassRepresentation(r, r.path)))

    case r: DefRepresentation => packagesSet + ((r.path, formatDefRepresentation(r, r.path)))

    case r: ValRepresentation => packagesSet + ((r.path, formatValRepresentation(r, r.path)))

    case _ => packagesSet
  }
}