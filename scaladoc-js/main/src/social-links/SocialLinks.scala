package dotty.tools.scaladoc

import org.scalajs.dom._
import org.scalajs.dom.ext._

import utils.HTML._

class SocialLinks:
  def addIcon(elem: html.Element) =
    elem.appendChild(
      img(src :=  s"${Globals.pathToRoot}images/${elem.getAttribute("data-icon-path")}")()
    )

  document.querySelectorAll(".social-icon").collect { case e: html.Element => e }.foreach(addIcon)
