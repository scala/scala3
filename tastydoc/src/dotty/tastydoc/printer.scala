package dotty.tastydoc

import representations._

def ParamListPrinter(paramList: ParamList) : String = paramList.list.map(x => x.title + ": " + x.tpe).mkString(
  "(" + (if(paramList.isImplicit) "implicit " else ""),
  ", ",
  ")"
)

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
      Md.codeBlock((if(r.modifiers.size > 0) r.modifiers.mkString("", " ", " ") else "") + "class " + r.name, "scala") +
      "\n" +
      Md.italics(r.comments) +
      "\n"
    }else{
      Md.header1("class " + r.name) +
      "\n" +
      (if (r.hasCompanion) Md.header2("Companion object : " + r.companionPath.mkString(".")) + "\n" else "") +
      Md.codeBlock((if(r.modifiers.size > 0) r.modifiers.mkString("", " ", " ") else "") + "class " + r.name + (if(r.typeParams.size > 0) r.typeParams.mkString("[", ", ", "]") else ""), "scala") +
      "\n" +
      r.comments +
      "\n" +
      Md.header2("Annotations:") +
      "\n" +
      r.annotations.mkString("\n") +
      "\n" +
      Md.header2("Constructors:")+
      r.constructors.map(x=>Md.codeBlock("def this" + x.paramLists.map(ParamListPrinter(_)).mkString(""), "scala")).mkString("") +
      "\n" +
      Md.header2("Members:") +
      "\n" +
      Md.header3("Definitions: ") +
      r.members.map(formatRepresentationToMarkdown(_, true)).mkString("") +
      "\n"
    }

  case r: DefRepresentation =>
    Md.codeBlock(
      (if(r.modifiers.size > 0) r.modifiers.mkString("", " ", " ") else "") +
      "def " +
      r.name +
      (if(r.typeParams.size > 0) r.typeParams.mkString("[", ", ", "]") else "") +
      r.paramLists.map(ParamListPrinter).mkString("") +
      ": " +
      r.returnValue, "scala") +
      r.comments +
      "\n"

  case r: ValRepresentation =>
    Md.codeBlock(
      (if(r.modifiers.size > 0) r.modifiers.mkString("", " ", " ") else "") +
      "val " +
      r.name +
      ": " +
      r.returnValue, "scala") +
      r.comments +
      "\n"

  case r: TypeRepresentation =>
      Md.codeBlock(
      (if(r.modifiers.size > 0) r.modifiers.mkString("", " ", " ") else "") +
      "type " +
      r.name +
      ": ", "scala") +
      r.comments +
      "\n"

  case _ : DebugRepresentation => "=============>ERROR<==============="
  case _ => ""
}