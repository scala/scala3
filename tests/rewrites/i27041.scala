//> using options -source:3.9-migration

// Already backticked: rewrite must not wrap again (scala/scala3#27041)
object `$foo` {}
enum Token { case `${` }

case object `$caseObj`
object `foo$bar`

enum CommaCases:
  case `$a`, `$b`

enum CaseClassEnum:
  case `Class$Case`(x: Int)

enum `$EnumName`:
  case Ok

given `$g`: Int = 1

package `$pkg`:
  val x = 1

val `$val` = 1
def `$def` = 2
class `$class`
type `$type` = Int
