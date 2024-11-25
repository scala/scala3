package dotty.tools.scaladoc.site.tags


import liqp.TemplateContext
import liqp.nodes.LNode
import liqp.tags.Tag
import scala.io.Source
import java.io.File
import scala.util.{Try, Success, Failure}
import scala.jdk.CollectionConverters._

class IncludeTag extends Tag("include") {

  override def render(context: TemplateContext, args: Array[? <: LNode]): Object = {
    // Render arguments and validate filename
    val filename = args(0).render(context) match {
      case s: String if s.endsWith(".html") => s
      case s: String => throw new IllegalArgumentException(s"{% include %} tag requires a string argument ending with .html, but got: $s")
      case _ => throw new IllegalArgumentException("{% include %} tag requires a string argument ending with .html")
    }

    val filePath = s"${IncludeTag.docsFolder}/$filename"
    println(s"Attempting to include file: $filePath")

    val inputData = args(1).render(context)
    context.put("include", inputData)

    try {
      // Read and parse the file content
      val fileContent = readFileContent(filePath) match {
        case Success(content) => content
        case Failure(exception) => throw new IllegalArgumentException(s"Error reading file '$filePath': ${exception.getMessage}")
      }

      // Resolve nested includes within the file content
      val resolvedContent = resolveNestedIncludes(fileContent, context)

      val siteData = context.getVariables().asInstanceOf[java.util.Map[String, Any]]
      // Parse the resolved content
      context.getParser().parse(resolvedContent).render(siteData)
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

  private def resolveNestedIncludes(content: String, context: TemplateContext): String = {
    // Use a regex to find all {% include %} tags in the content
    val includeTagPattern = "\\{%\\s*include\\s+\"([^\"]+)\"\\s*%\\}".r

    includeTagPattern.replaceAllIn(content, { matchData =>
      val includeFilename = matchData.group(1)
      val nestedFilePath = s"${IncludeTag.docsFolder}/$includeFilename"

      // Read and parse the nested file content
      val nestedFileContent = readFileContent(nestedFilePath) match {
        case Success(content) => content
        case Failure(exception) => throw new IllegalArgumentException(s"Error reading file '$nestedFilePath': ${exception.getMessage}")
      }

      // Recursively resolve any further nested includes within the nested file content
      val tempContext = new TemplateContext(context.getParser(), context.getVariables())
      resolveNestedIncludes(nestedFileContent, tempContext)
    })
  }
}

object IncludeTag {
  @volatile private var docsFolder: String = "_docs"

  def setDocsFolder(path: String): Unit = {
    docsFolder = path
  }

  def getDocsFolder: String = docsFolder
}
