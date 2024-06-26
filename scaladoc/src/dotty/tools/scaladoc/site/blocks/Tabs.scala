package dotty.tools.scaladoc.site.blocks


import liqp.TemplateContext
import liqp.nodes.LNode
import liqp.blocks.Block
import scala.collection.mutable.ListBuffer



class TabsBlock extends Block("tabs") {
  override def render(context: TemplateContext, nodes: LNode*): AnyRef = {
    val inputString = nodes.head.render(context).toString

    val pattern = """(.*?)(class=.*)""".r

    val (forValue, classValue) = inputString match {
      case pattern(forPart, classPart) => (forPart, classPart.stripPrefix("class="))
      case _ => ("", "")
    }

    // Render the content of the tabs block
    nodes.foreach(_.render(context))

    // Extract tab headers and contents from the context
    val tabHeaders = context.remove("tab-headers").asInstanceOf[ListBuffer[String]].mkString("\n")
    val tabContents = context.remove("tab-contents").asInstanceOf[ListBuffer[String]].mkString("\n")

    // Build the HTML string using StringBuilder
    val builder = new StringBuilder
    builder.append(
      s"""
         |<div class="tabs $classValue" data-for="$forValue">
         |  <div class="tab-headers">
         |    $tabHeaders
         |  </div>
         |  <div class="tab-contents">
         |    $tabContents
         |  </div>
         |</div>
         |""".stripMargin)

    // Return the final rendered string
    builder.toString
  }
}

class TabBlock extends Block("tab") {
  override def render(context: TemplateContext, nodes: LNode*): AnyRef = {
    val inputString = nodes.head.render(context).toString

    val pattern = """(.*)for=(.*)""".r

    val (tabName, forValue) = inputString match {
      case pattern(tab, forPart) => (tab, forPart)
      case _ => ("", "")
    }
    println(s"Rendering tab $tabName  $forValue")

    val content = nodes.tail.map(_.render(context).toString).mkString

    val header = s"""<button class="tab-link" data-tab="$tabName">$tabName</button>"""
    val tabContent =
      s"""
         |<div class="tab-content" id="$tabName">
         |  $content
         |</div>
         |""".stripMargin

    // Add the header and content to the context
    val headers = context.get("tab-headers")
    if (headers.isInstanceOf[ListBuffer[?]]) {
      val headersList = headers.asInstanceOf[ListBuffer[String]]
      headersList += header
    } else {
      val newHeaders = ListBuffer[String]()
      newHeaders += header
      context.put("tab-headers", newHeaders)
    }
    val contents = context.get("tab-contents")
    if (contents.isInstanceOf[ListBuffer[?]]) {
      val contentsList = contents.asInstanceOf[ListBuffer[String]]
      contentsList += tabContent
    } else {
      val newContents = ListBuffer[String]()
      newContents += tabContent
      context.put("tab-contents", newContents)
    }
    "" // Return empty string
  }
}
