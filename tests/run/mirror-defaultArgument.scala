import scala.deriving._
import scala.annotation.experimental

object Test extends App:

  case class WithDefault(x: Int, y: Int = 1)
  val m = summon[Mirror.Of[WithDefault]]
  assert(m.defaultArgument(1) == 1)
  try
    m.defaultArgument(0)
    throw IllegalStateException("There should be no default argument")
  catch
    case ex: NoSuchElementException => assert(ex.getMessage == "0") // Ok


  case class WithCompanion(s: String = "hello")
  case object WithCompanion // => mirrors must be anonymous

  val m2 = summon[Mirror.Of[WithCompanion]]
  assert(m2 ne WithCompanion)
  assert(m2.defaultArgument(0) == "hello")


  class Outer(val i: Int) {

    case class Inner(x: Int, y: Int = i + 1)
    case object Inner

    val m3 = summon[Mirror.Of[Inner]]
    assert(m3.defaultArgument(1) == i + 1)

    def localTest(d: Double): Unit = {
      case class Local(x: Int = i, y: Double = d, z: Double = i + d)
      case object Local

      val m4 = summon[Mirror.Of[Local]]
      assert(m4.defaultArgument(0) == i)
      assert(m4.defaultArgument(1) == d)
      assert(m4.defaultArgument(2) == i + d)
    }

  }

  val outer = Outer(3)
  val m5 = summon[Mirror.Of[outer.Inner]]
  assert(m5.defaultArgument(1) == 3 + 1)
  outer.localTest(9d)


  // new defaultArgument match tree should be able to unify different default value types
  case class Foo[T](x: Int = 0, y: String = "hi")

end Test
