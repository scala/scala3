package dotty.tools
package dotc
package core
package tasty

import ast.Trees._

object Splicing {
  import ast.tpd._

  type Splice = AnyRef /* tpd.Tree | tpd.Tree => tpd.Tree */

  case class Hole(args: List[Tree]) extends TermTree
}