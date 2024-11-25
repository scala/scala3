package dotty.tools.scaladoc.site.blocks


import liqp.TemplateContext
import liqp.nodes.LNode
import liqp.blocks.Block
import scala.collection.mutable.ListBuffer
import java.util.UUID

class TabsBlock extends Block("tabs") {
  override def render(context: TemplateContext, nodes: LNode*): AnyRef = {
    // Generate a unique ID for this set of tabs
    val uniqueId = UUID.randomUUID().toString

    // Add the unique ID to the context
    context.put("tabs-unique-id", uniqueId)

    // Render the content of the tabs block
    nodes.foreach(_.render(context))

    // Extract tab headers and contents from the context
    val tabHeaders = context.remove(s"tab-headers-$uniqueId").asInstanceOf[ListBuffer[String]].mkString("\n")
    val tabContents = context.remove(s"tab-contents-$uniqueId").asInstanceOf[ListBuffer[String]].mkString("\n")

    // Build the HTML string using StringBuilder
    val builder = new StringBuilder
    builder.append(
      s"""
         |<div class="tabs" id="tabs-$uniqueId">
         |  $tabHeaders
         |  $tabContents
         |</div>
         |""".stripMargin)

    // Return the final rendered string
    builder.toString
  }
}

class TabBlock extends Block("tab") {
  override def render(context: TemplateContext, nodes: LNode*): AnyRef = {
    val inputString = nodes.head.render(context).toString

    val pattern = """(.*?)(?:for=(.*))?""".r

    val (tabName, forValue) = inputString match {
      case pattern(tab, forPart) => (tab, Option(forPart).getOrElse(""))
      case _ => ("", "")
    }


    val content = nodes.tail.map(_.render(context).toString).mkString

    // Retrieve the unique ID from the context
    val uniqueId = context.get("tabs-unique-id").toString

    val header =
      s"""
         |<input type="radio" id="tab-$tabName-$uniqueId" name="tabs-$uniqueId" class="tab-radio">
         |<label for="tab-$tabName-$uniqueId" class="tab-label">$tabName</label>
         |""".stripMargin
    val tabContent =
      s"""
         |<div class="tab-content" id="content-$tabName-$uniqueId">
         |  $content
         |</div>
         |""".stripMargin

    // Add the header and content to the context
    context.get(s"tab-headers-$uniqueId") match {
      case headers: ListBuffer[String] @unchecked =>
        headers += header
      case null =>
        val newHeaders = ListBuffer[String]()
        newHeaders += header
        context.put(s"tab-headers-$uniqueId", newHeaders)
    }
    val contents = context.get(s"tab-contents-$uniqueId")
    if (contents.isInstanceOf[ListBuffer[?]]) {
      val contentsList = contents.asInstanceOf[ListBuffer[String]]
      contentsList += tabContent
    } else {
      val newContents = ListBuffer[String]()
      newContents += tabContent
      context.put(s"tab-contents-$uniqueId", newContents)
    }
    "" // Return empty string
  }
}
