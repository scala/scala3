package dotty.tools.scaladoc.site.blocks

import liqp.TemplateContext
import liqp.nodes.LNode
import liqp.TemplateContext
import liqp.nodes.LNode

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.{lang, util}
import java.util.Map as JMap
import java.util.HashMap


class AltDetails extends liqp.blocks.Block("altDetails") {
  def render(context: TemplateContext, nodes: LNode*): Any = {

    // Ensure the block content is the last node
    val block = nodes.last
    val inputString = nodes.dropRight(1).map(_.render(context).toString).mkString

    val (id, title, cssClass) = extractInfo(inputString)


    // Render the block content
    val blockContent = block.render(context).toString

    // Build the HTML string using StringBuilder
    val builder = new StringBuilder
    builder.append(
    s"""<div class="place-inline"><div id="$id" class="alt-details $cssClass"><input class="alt-details-control" type="checkbox" id="${id}__control" hidden aria-hidden="true"><label class="alt-details-toggle" for="${id}__control">$title</label><div class="alt-details-detail"><div class="wrap-tab">$blockContent</div></div></div></div>"""
    )

    // Return the final rendered string
    builder.toString
  }

  private def extractInfo(inputString: String): (String, String, String) = {
    val pattern = """^([^']+)'(.*?)'(?:class=([\w\-]+))?$""".r

    val matchResult = pattern.findFirstMatchIn(inputString)
    if (matchResult.isEmpty) {
      return ("", "", "") // Or return default values
    }

    val id = matchResult.get.group(1)
    val title = matchResult.get.group(2)
    val cssClass = matchResult.get.group(3)

    (id, title, cssClass)
  }
}
