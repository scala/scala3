package dotty.tools.dottydoc
package js
package html

import scalatags.JsDom.all._
import scalatags.generic
import scalatags.generic.Util
import org.scalajs.dom

object CustomTags {
  lazy val main = "main".tag[dom.html.Element]
  lazy val nav  = "nav".tag[dom.html.Element]
}
