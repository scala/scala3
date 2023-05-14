package dotty.tools.scaladoc


import org.scalajs.dom._
import org.scalajs.dom.ext._
import scala.scalajs.js
import scalajs.js.DynamicImplicits.truthValue


// Handful of links
// https://css-tricks.com/popping-hidden-overflow/
// https://github.com/highlightjs/highlight.js/issues/728
// https://stackoverflow.com/a/54482317/14320995
class TooltipNormalizer:
  document.addEventListener("mouseover", (e: Event) => {
    if e.target.asInstanceOf[js.Dynamic].closest(".snippet-error.tooltip") then
      val tooltipParent = e.target.asInstanceOf[html.Span]
      val tooltipContainer = tooltipParent.querySelector(".tooltip-container")
      js.typeOf(tooltipContainer) match
        case "undefined" =>
        case _ =>
          tooltipContainer match
            case casted: html.Span => casted.style = s"left: ${Math.round(tooltipParent.offsetLeft)}"
            case _ =>
  })
