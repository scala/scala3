package dotty.tastydoc

import mdscala.Md

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