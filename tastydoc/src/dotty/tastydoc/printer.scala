package dotty.tastydoc

import representations._
import comment.Comment

def formatParamList(paramList: ParamList) : String = paramList.list.map(x => x.title + ": " + x.tpe).mkString(
  "(" + (if(paramList.isImplicit) "implicit " else ""),
  ", ",
  ")"
)

def formatComments(comment: Option[Comment]) : String = comment match {
  case Some(c) =>
    c.body +
    "\n\n" +
    (if(c.authors.nonEmpty) Md.bold(Md.italics("authors")) + " " + c.authors.mkString(", ") + "\n" else "") +
    (if(c.see.nonEmpty) Md.bold(Md.italics("see")) + " "  + c.see.mkString(", ") + "\n" else "") +
    (if(c.result.isDefined) Md.bold(Md.italics("return")) + " "  + c.result.get + "\n" else "") +
    (if(c.throws.nonEmpty) c.throws.map((x, y) => Md.bold(Md.italics(x)) + " " + y).mkString("\n") + "\n" else "") +
    (if(c.valueParams.nonEmpty) c.valueParams.map((x, y) => Md.bold(Md.italics(x)) + " " + y).mkString("\n") + "\n" else "") +
    (if(c.typeParams.nonEmpty) c.typeParams.map((x, y) => Md.bold(Md.italics(x)) + " " + y).mkString("\n") + "\n" else "") +
    (if(c.version.isDefined) Md.bold(Md.italics("version")) + " "  + c.version.get + "\n" else "") +
    (if(c.since.isDefined) Md.bold(Md.italics("since")) + " "  + c.since.get + "\n" else "") +
    (if(c.todo.nonEmpty) Md.bold(Md.italics("TODO")) + " " + c.todo.mkString(", ") + "\n" else "") +
    (if(c.deprecated.isDefined) Md.bold(Md.italics("deprecated")) + " "  + c.deprecated.get + "\n" else "") +
    (if(c.note.nonEmpty) Md.bold(Md.italics("Note")) + " " + c.note.mkString("\n") + "\n" else "") +
    (if(c.example.nonEmpty) Md.bold(Md.italics("Example")) + " " + c.example.mkString("\n") + "\n" else "") +
    (if(c.constructor.isDefined) Md.bold(Md.italics("Constructor")) + " "  + c.constructor.get + "\n" else "") +
    (if(c.group.isDefined) Md.bold(Md.italics("Group")) + " "  + c.group.get + "\n" else "") +
    (if(c.groupDesc.nonEmpty) c.groupDesc.map((x, y) => Md.bold(Md.italics(x)) + " " + y).mkString("\n") + "\n" else "") +
    (if(c.groupNames.nonEmpty) c.groupNames.map((x, y) => Md.bold(Md.italics(x)) + " " + y).mkString("\n") + "\n" else "") +
    (if(c.groupPrio.nonEmpty) c.groupPrio.map((x, y) => Md.bold(Md.italics(x)) + " " + y).mkString("\n") + "\n" else "") +
    (if(c.hideImplicitConversions.nonEmpty) Md.bold(Md.italics("Hide Implicit Conversions")) + " " + c.hideImplicitConversions.mkString(", ") + "\n" else "")
  case None => ""
}

def formatRepresentationToMarkdown(representation: Representation, insideClassOrObject: Boolean) : String = representation match {
  //case _ => ""
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