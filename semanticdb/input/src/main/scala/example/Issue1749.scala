// See https://github.com/scalameta/scalameta/issues/1749
package example

import scala.math.Ordered.orderingToOrdered

class Issue1749 {
  val x1 = 42
  val x2 = 42
  (x1, x1)
    .compare((x2, x2))
}
