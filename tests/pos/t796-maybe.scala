//> using options -Yexplicit-nulls
/** I know what I am doing is wrong -- since I am about to look into
 *  this bug, I add a test in pending/pos... however, I am afraid that
 *  once this bug is fixed, this test case might go into test/pos
 *  there it adds to the huge number of tiny little test cases.
 *
 * Ideally, an option in the bugtracking system would automatically
 * handle "pos" bugs.
 */
import language.experimental.magic
object Test extends App {

  object Twice {
    def apply(x: Int) = x * 2
    def unapply(x: Int): Tuple1[Int]? =
      if (x % 2 == 0) Tuple1(x / 2)
      else null
  }

  def test(x: Int) = x match {
    case Twice(y) => "x is two times " + y
    case _ => "x is odd"
  }

  Console.println(test(3))
  Console.println(test(4))

}
