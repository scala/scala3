/* These tests show various mechanisms available for implicit prioritization.
 */
import language.`3.6`

class A  // The type for which we infer terms below
class B extends A

/* First, two schemes that require a pre-planned architecture for how and
 * where given instances are defined.
 *
 * Traditional scheme: prioritize with location in class hierarchy
 */
class LowPriorityImplicits:
  given g1: A()

object NormalImplicits extends LowPriorityImplicits:
  given g2: B()

def test1 =
  import NormalImplicits.given
  val x = summon[A]
  val _: B = x
  val y = summon[B]
  val _: B = y
