// prelude.scala

import prelude.newtypes.Natural

package object prelude extends ZSetSyntax {
  type MultiSet[+A] = ZSet[A, Natural]
  val MultiSet: ZSet.type = ZSet
}