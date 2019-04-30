package dotty.tastydoc

import representations._
import comment.Comment

import java.io._

object DocPrinter{
  private def formatParamList(paramList: ParamList) : String = paramList.list.map(x => x.title + ": " + x.tpe).mkString(
    "(" + (if(paramList.isImplicit) "implicit " else ""),
    ", ",
    ")"
  )

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

  def formatRepresentationToMarkdown(representation: Representation, insideClassOrObject: Boolean) : String = representation match {
    case r : PackageRepresentation =>
      r.name +
      "\n" +
      r.members.map(formatRepresentationToMarkdown(_, insideClassOrObject)).mkString("\n")

    case r: ImportRepresentation =>
      "import " +
      r.path.mkString("", ".", ".") +
      r.name +
      "\n"

    case r: ClassRepresentation =>
      if(insideClassOrObject){
        Md.codeBlock((if(r.modifiers.nonEmpty) r.modifiers.mkString("", " ", " ") else "") + "class " + r.name, "scala") +
        "\n" +
        formatComments(r.comments) +
        "\n"
      }else{
        Md.header1("class " + r.name) +
        "\n" +
        (if (r.hasCompanion) Md.header2("Companion object : " + r.companionPath.mkString(".")) + "\n" else "") +
        Md.codeBlock((if(r.modifiers.nonEmpty) r.modifiers.mkString("", " ", " ") else "") +
          "class " +
          r.name +
          (if(r.typeParams.nonEmpty) r.typeParams.mkString("[", ", ", "]") else "") +
          (if(r.parents.nonEmpty) " extends " + r.parents.head + r.parents.tail.map(" with " + _).mkString("") else ""), "scala") +
        "\n" +
        formatComments(r.comments) +
        "\n" +
        Md.header2("Annotations:") +
        "\n" +
        r.annotations.mkString("\n") +
        "\n" +
        Md.header2("Constructors:")+
        r.constructors.map(x=>Md.codeBlock(r.name + x.paramLists.map(formatParamList(_)).mkString(""), "scala")).mkString("") +
        "\n" +
        Md.header2("Members:") +
        "\n" +
        Md.header3("Definitions: ") +
        r.members.flatMap{case x : DefRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true)).mkString("") +
        "\n" +
        Md.header3("Values: ") +
        r.members.flatMap{case x : ValRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true)).mkString("") +
        "\n" +
        Md.header3("Types: ") +
        r.members.flatMap{case x : TypeRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true)).mkString("") +
        "\n" +
        Md.header3("Classes: ") +
        r.members.flatMap{case x : ClassRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true)).mkString("") +
        "\n" +
        Md.header3("Objects: ") +
        r.members.flatMap{case x : ObjectRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true)).mkString("") +
        "\n"
      }

    case r: ObjectRepresentation =>
      if(insideClassOrObject){
        Md.codeBlock((if(r.modifiers.nonEmpty) r.modifiers.mkString("", " ", " ") else "") + "object " + r.name, "scala") +
        "\n" +
        formatComments(r.comments) +
        "\n"
      }else{
        Md.header1("object " + r.name) +
        "\n" +
        (if (r.hasCompanion) Md.header2("Companion class : " + r.companionPath.mkString(".")) + "\n" else "") +
        Md.codeBlock((if(r.modifiers.nonEmpty) r.modifiers.mkString("", " ", " ") else "") + "class " + r.name, "scala") +
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
        r.members.flatMap{case x : DefRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true)).mkString("") +
        "\n" +
        Md.header3("Values: ") +
        r.members.flatMap{case x : ValRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true)).mkString("") +
        "\n" +
        Md.header3("Types: ") +
        r.members.flatMap{case x : TypeRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true)).mkString("") +
        "\n" +
        Md.header3("Classes: ") +
        r.members.flatMap{case x : ClassRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true)).mkString("") +
        "\n" +
        Md.header3("Objects: ") +
        r.members.flatMap{case x : ObjectRepresentation => Some(x) case _ => None}.map(x => Md.header4(x.name) + formatRepresentationToMarkdown(x, true)).mkString("") +
        "\n"
      }

    case r: DefRepresentation =>
      Md.codeBlock(
        (if(r.modifiers.nonEmpty) r.modifiers.mkString("", " ", " ") else "") +
        "def " +
        r.name +
        (if(r.typeParams.nonEmpty) r.typeParams.mkString("[", ", ", "]") else "") +
        r.paramLists.map(formatParamList).mkString("") +
        ": " +
        r.returnValue, "scala") +
        "\n" +
        formatComments(r.comments) +
        "\n"

    case r: ValRepresentation =>
      Md.codeBlock(
        (if(r.modifiers.nonEmpty) r.modifiers.mkString("", " ", " ") else "") +
        "val " +
        r.name +
        ": " +
        r.returnValue, "scala") +
        "\n" +
        formatComments(r.comments) +
        "\n"

    case r: TypeRepresentation =>
        Md.codeBlock(
        (if(r.modifiers.nonEmpty) r.modifiers.mkString("", " ", " ") else "") +
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
      pw.write(formatRepresentationToMarkdown(r, false))
      pw.close
      if(r.path.nonEmpty){
        packagesSet + ((r.path, "class " + Md.link(r.name, "./" + r.path.last + "/" + r.name + ".md")))
      }else{
        packagesSet + ((r.path, "class " + Md.link(r.name, "./" + r.name + ".md")))
      }

    case r: ObjectRepresentation =>
      val file = new File("./" + folderPrefix + r.path.mkString("", "/", "/") + r.name + ".md")
      file.getParentFile.mkdirs
      val pw = new PrintWriter(file)
      pw.write(formatRepresentationToMarkdown(r, false))
      pw.close
      if(r.path.nonEmpty){
        packagesSet + ((r.path, "object " + Md.link(r.name, "./" + r.path.last + "/" + r.name + ".md")))
      }else{
        packagesSet + ((r.path, "object " + Md.link(r.name, "./" + r.name + ".md")))
      }
    case _ => packagesSet
  }
}