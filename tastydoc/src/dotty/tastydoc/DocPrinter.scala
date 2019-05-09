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

  private def makeLink(label: String, link: String, hasOwnFile: Boolean, linkScala: Boolean = false): String = {
    if(hasOwnFile){
      "<a href=\"" + link + "/" + label + ".md\">" + label + "</a>"
    } else if(link == "."){
      "<a href=\"#" + label + "\">" + label + "</a>"
    } else{
      "<a href=\"" + link + ".md#" + label + "\">" + label + "</a>"
    }
  }

  private def formatReferences(reference: Reference, declarationPath: List[String]) : String = reference match {
    case TypeReference(label, link, typeParams, hasOwnFile) =>
      @tailrec
      def ascendPath(path: List[String], link: List[String]): String = path match {
        case x::xs if link.nonEmpty && link.head == x => ascendPath(xs, link.tail)
        case _ => (if(path.isEmpty) "." else path.map(_ => "..").mkString("/")) + (if(link.isEmpty) "" else link.mkString("/", "/", ""))
      }

      val relativeLink = {
        if(link == "."){
          "."
        }else{
          ascendPath(declarationPath, link.split("/").toList.tail)
        }
      }

      if(typeParams.isEmpty){
        makeLink(label, relativeLink, hasOwnFile)
      }else{
        makeLink(label, relativeLink, hasOwnFile) + typeParams.map(formatReferences(_, declarationPath)).mkString("[", ", ", "]")
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
      (if(c.throws.nonEmpty) c.throws.map((x, y) => Md.bold(Md.italics(x)) + " " + y).mkString("\n") else "") +
      (if(c.valueParams.nonEmpty) c.valueParams.map((x, y) => Md.bold(Md.italics(x)) + " " + y).mkString("\n") else "") +
      (if(c.typeParams.nonEmpty) c.typeParams.map((x, y) => Md.bold(Md.italics(x)) + " " + y).mkString("\n") else "") +
      (if(c.version.isDefined) Md.bold(Md.italics("version")) + " "  + c.version.get else "") +
      (if(c.since.isDefined) Md.bold(Md.italics("since")) + " "  + c.since.get else "") +
      (if(c.todo.nonEmpty) Md.bold(Md.italics("TODO")) + " " + c.todo.mkString(", ") else "") +
      (if(c.deprecated.isDefined) Md.bold(Md.italics("deprecated")) + " "  + c.deprecated.get else "") +
      (if(c.note.nonEmpty) Md.bold(Md.italics("Note")) + " " + c.note.mkString("\n") else "") +
      (if(c.example.nonEmpty) Md.bold(Md.italics("Example")) + " " + c.example.mkString("\n") else "") +
      (if(c.constructor.isDefined) Md.bold(Md.italics("Constructor")) + " "  + c.constructor.get else "") +
      (if(c.group.isDefined) Md.bold(Md.italics("Group")) + " "  + c.group.get else "") +
      (if(c.groupDesc.nonEmpty) c.groupDesc.map((x, y) => Md.bold(Md.italics(x)) + " " + y).mkString("\n") else "") +
      (if(c.groupNames.nonEmpty) c.groupNames.map((x, y) => Md.bold(Md.italics(x)) + " " + y).mkString("\n") else "") +
      (if(c.groupPrio.nonEmpty) c.groupPrio.map((x, y) => Md.bold(Md.italics(x)) + " " + y).mkString("\n") else "") +
      (if(c.hideImplicitConversions.nonEmpty) Md.bold(Md.italics("Hide Implicit Conversions")) + " " + c.hideImplicitConversions.mkString(", ") else "")
    case None => ""
  }

  def formatRepresentationToMarkdown(representation: Representation, insideClassOrObject: Boolean, declarationPath: List[String]) : String = representation match {
    case r : PackageRepresentation =>
      r.name +
      "\n" +
      r.members.map(formatRepresentationToMarkdown(_, insideClassOrObject, r.path)).mkString("\n")

    case r: ImportRepresentation =>
      "import " +
      r.path.mkString("", ".", ".") +
      r.name +
      "\n"

    case r: ClassRepresentation if r.isTrait =>
      if(insideClassOrObject){
        htmlPreCode(formatModifiers(r.modifiers, r.privateWithin, r.protectedWithin, declarationPath) + "trait " + r.name, "scala") +
        "\n" +
        formatComments(r.comments) +
        "\n"
      }else{
        Md.header1("trait " + r.name) +
        "\n" +
        (if (r.hasCompanion) Md.header2("Companion object : " + r.companionPath.mkString(".")) + "\n" else "") +
        htmlPreCode(formatModifiers(r.modifiers, r.privateWithin, r.protectedWithin, r.path) +
          "trait " +
          r.name +
          (if(r.typeParams.nonEmpty) r.typeParams.mkString("[", ", ", "]") else "") +
          (if(r.parents.nonEmpty) " extends " + formatReferences(r.parents.head, r.path) + r.parents.tail.map(" with " + formatReferences(_, r.path)).mkString("") else "")
          ,"scala") +
        "\n" +
        formatComments(r.comments) +
        "\n" +
        Md.header2("Annotations:") +
        "\n" +
        r.annotations.mkString("\n") +
        "\n" +
        Md.header2("Members:") +
        "\n" +
        Md.header3("Definitions: ") +
        r.members.flatMap{case x : DefRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true, r.path)).mkString("") +
        "\n" +
        Md.header3("Values: ") +
        r.members.flatMap{case x : ValRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true, r.path)).mkString("") +
        "\n" +
        Md.header3("Types: ") +
        r.members.flatMap{case x : TypeRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true, r.path)).mkString("") +
        "\n" +
        Md.header3("Classes: ") +
        r.members.flatMap{case x : ClassRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true, r.path)).mkString("") +
        "\n" +
        Md.header3("Objects: ") +
        r.members.flatMap{case x : ObjectRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true, r.path)).mkString("") +
        "\n"
      }

    case r: ClassRepresentation =>
      if(insideClassOrObject){
        htmlPreCode(formatModifiers(r.modifiers, r.privateWithin, r.protectedWithin, declarationPath) + (if(r.isCase) "case " else "") + "class " + r.name, "scala") +
        "\n" +
        formatComments(r.comments) +
        "\n"
      }else{
        Md.header1("class " + r.name) +
        "\n" +
        (if (r.hasCompanion) Md.header2("Companion object : " + r.companionPath.mkString(".")) + "\n" else "") +
        htmlPreCode(formatModifiers(r.modifiers, r.privateWithin, r.protectedWithin, r.path) +
          (if(r.isCase) "case " else "") +
          "class " +
          r.name +
          (if(r.typeParams.nonEmpty) r.typeParams.mkString("[", ", ", "]") else "") +
          (if(r.parents.nonEmpty) " extends " + formatReferences(r.parents.head, r.path) + r.parents.tail.map(" with " + formatReferences(_, r.path)).mkString("") else "")
          , "scala") +
        "\n" +
        formatComments(r.comments) +
        "\n" +
        Md.header2("Annotations:") +
        "\n" +
        r.annotations.mkString("\n") +
        "\n" +
        Md.header2("Constructors:")+
        r.constructors.map((ls, com) => htmlPreCode(r.name + ls.paramLists.map(formatParamList(_, r.path)).mkString(""), "scala") + "\n" + formatComments(com)).mkString("") +
        "\n" +
        Md.header2("Members:") +
        "\n" +
        Md.header3("Definitions: ") +
        r.members.flatMap{case x : DefRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true, r.path)).mkString("") +
        "\n" +
        Md.header3("Values: ") +
        r.members.flatMap{case x : ValRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true, r.path)).mkString("") +
        "\n" +
        Md.header3("Types: ") +
        r.members.flatMap{case x : TypeRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true, r.path)).mkString("") +
        "\n" +
        Md.header3("Classes: ") +
        r.members.flatMap{case x : ClassRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true, r.path)).mkString("") +
        "\n" +
        Md.header3("Objects: ") +
        r.members.flatMap{case x : ObjectRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true, r.path)).mkString("") +
        "\n"
      }

    case r: ObjectRepresentation =>
      if(insideClassOrObject){
        htmlPreCode(
          formatModifiers(r.modifiers, r.privateWithin, r.protectedWithin, declarationPath) +
          (if(r.isCase) "case " else "") +
          "object " +
          r.name +
          (if(r.parents.nonEmpty) " extends " + formatReferences(r.parents.head, declarationPath) + r.parents.tail.map(" with " + formatReferences(_, declarationPath)).mkString("") else "")
          , "scala") +
        "\n" +
        formatComments(r.comments) +
        "\n"
      }else{
        Md.header1("object " + r.name) +
        "\n" +
        (if (r.hasCompanion) Md.header2("Companion class : " + r.companionPath.mkString(".")) + "\n" else "") +
        htmlPreCode(formatModifiers(r.modifiers, r.privateWithin, r.protectedWithin, r.path) + (if(r.isCase) "case " else "") + "object " + r.name, "scala") +
        "\n" +
        formatComments(r.comments) +
        "\n" +
        Md.header2("Annotations:") +
        "\n" +
        r.annotations.mkString("\n") +
        "\n" +
        Md.header2("Members:") +
        "\n" +
        Md.header3("Definitions: ") +
        r.members.flatMap{case x : DefRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true, r.path)).mkString("") +
        "\n" +
        Md.header3("Values: ") +
        r.members.flatMap{case x : ValRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true, r.path)).mkString("") +
        "\n" +
        Md.header3("Types: ") +
        r.members.flatMap{case x : TypeRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true, r.path)).mkString("") +
        "\n" +
        Md.header3("Classes: ") +
        r.members.flatMap{case x : ClassRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true, r.path)).mkString("") +
        "\n" +
        Md.header3("Objects: ") +
        r.members.flatMap{case x : ObjectRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true, r.path)).mkString("") +
        "\n"
      }

    case r: DefRepresentation =>
      htmlPreCode(
        formatModifiers(r.modifiers, r.privateWithin, r.protectedWithin, declarationPath) +
        "def " +
        r.name +
        (if(r.typeParams.nonEmpty) r.typeParams.mkString("[", ", ", "]") else "") +
        r.paramLists.map(formatParamList(_, declarationPath)).mkString("") +
        ": " +
        formatReferences(r.returnValue, declarationPath), "scala") +
        "\n" +
        formatComments(r.comments) +
        "\n"

    case r: ValRepresentation =>
      htmlPreCode(
        formatModifiers(r.modifiers, r.privateWithin, r.protectedWithin, declarationPath) +
        "val " +
        r.name +
        ": " +
        formatReferences(r.returnValue, declarationPath), "scala") +
        "\n" +
        formatComments(r.comments) +
        "\n"

    case r: TypeRepresentation =>
        htmlPreCode(
        formatModifiers(r.modifiers, r.privateWithin, r.protectedWithin, declarationPath) +
        "type " +
        r.name +
        ": ", "scala") +
        "\n" +
        formatComments(r.comments) +
        "\n"

    case _ : DebugRepresentation => "=============>ERROR<==============="
    case _ => ""
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
      pw.write(formatRepresentationToMarkdown(r, false, r.path))
      pw.close

      val kind = if(r.isTrait) "trait" else "class"

      if(r.path.nonEmpty){
        packagesSet + ((r.path, kind + " " + Md.link(r.name, "./" + r.path.last + "/" + r.name + ".md")))
      }else{
        packagesSet + ((r.path, kind + " " + Md.link(r.name, "./" + r.name + ".md")))
      }

    case r: ObjectRepresentation =>
      val file = new File("./" + folderPrefix + r.path.mkString("", "/", "/") + r.name + ".md")
      file.getParentFile.mkdirs
      val pw = new PrintWriter(file)
      pw.write(formatRepresentationToMarkdown(r, false, r.path))
      pw.close
      if(r.path.nonEmpty){
        packagesSet + ((r.path, "object " + Md.link(r.name, "./" + r.path.last + "/" + r.name + ".md")))
      }else{
        packagesSet + ((r.path, "object " + Md.link(r.name, "./" + r.name + ".md")))
      }
    case _ => packagesSet
  }
}