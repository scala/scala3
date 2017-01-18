package dotty.tools
package dottydoc
package staticsite

import java.util.{ Map => JMap }

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
}
