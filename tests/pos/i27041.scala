//> using options -Werror -source:3.9

// 1:1 reproduction from scala/scala3#27041
object `$foo` {}
enum Token { case `${` }

// Adjacent: other backticked $ definitions that should also be exempt
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

// Regression anchors: attachment-based exemption already works for these
val `$val` = 1
def `$def` = 2
class `$class`
type `$type` = Int
