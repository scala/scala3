package dotty.tastydoc

import mdscala.Md

def format(container: Container, level: Int) : String = container match {
  case PackageContainer(sign, content) =>
    (0 until level).map(_ => "  ").foldLeft("")(_+_) +
    sign +
    "\n" +
    content.map(format(_, level+1)).foldLeft("")(_+_) +
    "\n"
  case ImportContainer(sign) =>
    (0 until level).map(_ => "  ").foldLeft("")(_+_) +
    sign +
    "\n"
  case ClassContainer(sign, defdef, valdef, typedef) =>
    (0 until level).map(_ => "  ").foldLeft("")(_+_) +
    sign +
    "\n" +
    (0 until level).map(_ => "  ").foldLeft("")(_+_) +
    "Methods :" +
    "\n" +
    defdef.map(format(_, level+1)).foldLeft("")(_+_) +
    (0 until level).map(_ => "  ").foldLeft("")(_+_) +
    "Values :" +
    "\n" +
    valdef.map(format(_, level+1)).foldLeft("")(_+_) +
    (0 until level).map(_ => "  ").foldLeft("")(_+_) +
    "Types :" +
    "\n" +
    typedef.map(format(_, level+1)).foldLeft("")(_+_) +
    "\n"
  case TypeContainer(sign) =>
    (0 until level).map(_ => "  ").foldLeft("")(_+_) +
    sign +
    "\n"
  case DefContainer(sign) =>
    (0 until level).map(_ => "  ").foldLeft("")(_+_) +
    sign +
    "\n"
  case ValContainer(sign) =>
    (0 until level).map(_ => "  ").foldLeft("")(_+_) +
    sign +
    "\n"

  case MissingMatchContainer() => "No Match"
}

def formatToMarkdown(container: Container, level: Int) : String = container match {
  case PackageContainer(sign, content) =>
    Md.header(sign, level + 2) +
    content.map(formatToMarkdown(_, level)).foldLeft("")(_+_)
  case ImportContainer(sign) =>
    Md.header(sign, level + 3)
  case ClassContainer(sign, defdef, valdef, typedef) =>
    Md.header(sign, level + 1) + //TODO: Bound level everywhere
    Md.header("Methods :", level + 2) +
    defdef.map(formatToMarkdown(_, level+1)).foldLeft("")(_+_) +
    Md.header("Values :", level + 2) +
    valdef.map(formatToMarkdown(_, level+1)).foldLeft("")(_+_) +
    Md.header("Types :", level+2) +
    typedef.map(formatToMarkdown(_, level+1)).foldLeft("")(_+_)
  case TypeContainer(sign) =>
    Md.codeBlock(sign, "scala")
  case DefContainer(sign) =>
    Md.codeBlock(sign, "scala")
  case ValContainer(sign) =>
    Md.codeBlock(sign, "scala")

  case MissingMatchContainer() => "No Match"
}