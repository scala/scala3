package dotty.tools.scaladoc.site.tags


class IncludeTag extends Tag("include") {
  private val docsFolder = "_docs"

  override def render(context: TemplateContext, args: Array[? <: LNode]): Object = {


    // Render arguments and validate filename
    val filename = args(0).render(context) match {
      case s: String if s.endsWith(".html") => s
      case s: String => throw new IllegalArgumentException(s"{% include %} tag requires a string argument ending with .html, but got: $s")
      case _ => throw new IllegalArgumentException("{% include %} tag requires a string argument ending with .html")
    }

    val filePath = s"$docsFolder/$filename"
    println(s"Attempting to include file: $filePath")

    val inputData = args(1).render(context)
    context.put("include", inputData)



    try {
      // Read and parse the file content
      val fileContent = readFileContent(filePath) match {
        case Success(content) => content
        case Failure(exception) => throw new IllegalArgumentException(s"Error reading file '$filePath': ${exception.getMessage}")
      }


      val siteData = context.getVariables().asInstanceOf[java.util.Map[String, Any]]
      // Parse the file content
      context.getParser().parse(fileContent).render(siteData)

    } finally {
      // Clean up: remove the input data from the context
      context.remove("include")
    }
  }

  private def readFileContent(filePath: String): Try[String] = {
    Try {
      val file = new File(filePath)
      println(s"Resolved file path: ${file.getAbsolutePath}")
      if (!file.exists()) {
        throw new IllegalArgumentException(s"File '$filePath' does not exist")
      }
      val source = Source.fromFile(file)
      try source.mkString finally source.close()
    }
  }
}
