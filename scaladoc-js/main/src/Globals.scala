package dotty.tools.scaladoc

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope

@js.native
@JSGlobalScope
object Globals extends js.Object {
  val pathToRoot: String = js.native
  val versionsDictionaryUrl: String = js.native
}

object StringUtils {
  def createCamelCaseTokens(s: String): List[String] =
    if s.isEmpty then List.empty
    else if s.tail.indexWhere(_.isUpper) == -1 then List(s)
    else List(s.take(s.tail.indexWhere(_.isUpper) + 1)) ++ createCamelCaseTokens(s.drop(s.tail.indexWhere(_.isUpper) + 1))
}
