package dotty.tools
package dottydoc
package staticsite

import java.util.{ Map => JMap, List => JList }

import com.fasterxml.jackson.databind.ObjectMapper
import liqp.filters.Filter

/** Custom liquid template filters */
object filters {

  /** Used to reverse arrays:
    *
    * ```html
    * {% assign array = "1,2,3,4,5" | split: "," %}
    * {{ array | reverse }}
    * ```
    */
  final class Reverse extends Filter("reverse") {
    override def apply(value: Any, params: AnyRef*): AnyRef = {
      val array = super.asArray(value)
      if (array.length == 0) null
      else array.reverse
    }
  }

  /** Used to get the first element of arrays and strings:
    *
    * ```html
    * {% assign array = "1,2,3,4,5" | split: "," %}
    * {{ array | first }}
    * ```
    * The above snippet will render "1"
    */
  final class First extends Filter("first") {
    override def apply(value: Any, params: AnyRef*): AnyRef = value match {
      case str: String if str.nonEmpty => str.charAt(0).toString
      case xs: Array[String] if xs.nonEmpty => xs.head
      case _ => null
    }
  }

  /** Used to transform java representation to valid JSON
   *
   *  This can be used on things like the docs (`java.util.List[Map[String,_]]`)
   *  which are available globally:
   *
   *  ```html
   *  {{ docs | json }}
   *  ```
   */
  final class Json extends Filter("json") {
    override def apply(value: Any, params: AnyRef*): AnyRef = value match {
      case map: JList[_] => new ObjectMapper().writeValueAsString(map)
      case _ => null
    }
  }
}
