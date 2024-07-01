// ZSet.scala
// moving it to core.scala would lead to Recursion limit exceeded: find-member prelude.ZSetSyntax
package prelude

import prelude.newtypes._

class ZSet[+A, +B]
object ZSet
trait ZSetSyntax {
  implicit final class ZSetMapOps[+A](self: Map[A, Natural])
}