package dotty.tools
package dottydoc
package staticsite

import java.util.{ List => JList, Map => JMap }

/** A simple wrapper for Maps to impose a certain degree of typesafety */
object MapOperations {
  implicit class SaferMap(val map: Map[String, AnyRef]) extends AnyVal {
    // `AnyRef` is actually `String | JList[String]`
    def getString(str: String): Option[String] = map.get(str).flatMap {
      case res: String => Some(res)
      case _ => None
    }

    def getList(str: String): Option[JList[String]] = map.get(str).flatMap {
      case res: JList[String] @unchecked => Some(res)
      case _ => None
    }

    def string(str: String): String = getString(str).getOrElse("")
    def list(str: String): JList[String] = getList(str).getOrElse(new java.util.ArrayList[String])
  }
}

