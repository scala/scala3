package dotty.tastydoc

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