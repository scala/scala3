package dotty.tastydoc

def formatToMarkdown(container: Container, level: Int) : String = {

  def formatDoc(doc: String) : String = {
    val lines = doc.split("\n").map(_+"\n")
      .map(_.replaceFirst(" *\\* *| *\\/\\*\\* *", "").replaceAll(" *\\*/ *", ""))
    //TODO : params on multiple lines
    val params = lines.filter(_.startsWith("@param"))
      .map(s => s.substring("@param".length, s.length).dropWhile(_ == ' '))
      .map(s => (s.takeWhile(_ != ' '), s.dropWhile(_ != ' ').dropWhile(_ == ' ')))
      .foldLeft(""){case (acc, (p, desc)) => acc + Md.bold(p) + "\t" + desc + "\n"}

    val returns = lines.filter(_.startsWith("@return"))
      .map(s => Md.bold("return") + " " + s.substring("@return".length, s.length).dropWhile(_ == ' '))
      .foldLeft("")(_+_+"\n")

    val noAnnotationLines = lines.filter(s => !s.startsWith("@param") && !s.startsWith("@return")).foldLeft("")(_+_)

    noAnnotationLines +
    (if(params == "") "" else "\n" + params) +
    (if(returns == "") "" else "\n" + returns)
  }

  container match {
    case PackageContainer(sign, content, userDoc) =>
      Md.header(sign, level + 2) +
      content.map(formatToMarkdown(_, level)).foldLeft("")(_+_) +
      formatDoc(userDoc)
    case ImportContainer(sign, userDoc) =>
      formatDoc(userDoc) +
      Md.header(sign, level + 3)
    case ClassContainer(sign, defdef, valdef, typedef, userDoc) =>
      Md.header(sign, level + 1) + //TODO: Bound level everywhere
      formatDoc(userDoc) +
      Md.header("Methods :", level + 2) +
      defdef.map(formatToMarkdown(_, level+1)).foldLeft("")(_+_) +
      Md.header("Values :", level + 2) +
      valdef.map(formatToMarkdown(_, level+1)).foldLeft("")(_+_) +
      Md.header("Types :", level+2) +
      typedef.map(formatToMarkdown(_, level+1)).foldLeft("")(_+_)
    case TypeContainer(sign, userDoc) =>
      Md.codeBlock(sign, "scala") +
      formatDoc(userDoc)
    case DefContainer(sign, userDoc) =>
      Md.codeBlock(sign, "scala") +
      formatDoc(userDoc)
    case ValContainer(sign, userDoc) =>
      Md.codeBlock(sign, "scala") +
      formatDoc(userDoc)

    case MissingMatchContainer() => "Documentation missmatch"
  }
}

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
      Md.codeBlock((if(r.modifiers.size > 0) r.modifiers.mkString("", " ", " ") else "") + "class " + r.name, "scala") +
      "\n" +
      r.comments +
      "\n" +
      Md.header2("Members :") +
      "\n" +
      r.members.map(formatRepresentationToMarkdown(_, true)).mkString("") +
      "\n"
    }

  case r: DefRepresentation =>
    Md.codeBlock(
      (if(r.modifiers.size > 0) r.modifiers.mkString("", " ", " ") else "") +
      "def " +
      r.name +
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

  case _ : DebugRepresentation => "=============>ERROR<==============="
  case _ => ""
}