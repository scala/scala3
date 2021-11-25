package dotty.tools.scaladoc

import org.scalajs.dom._
import org.scalajs.dom.ext._

class SocialLinks:
  def addIcon(elem: html.Element) =
    val img = document.createElement("img").asInstanceOf[html.Image]
    img.src = s"${Globals.pathToRoot}images/${elem.getAttribute("data-icon-path")}"
    elem.appendChild(img)

  document.querySelectorAll(".social-icon").collect { case e: html.Element => e }.foreach(addIcon)
