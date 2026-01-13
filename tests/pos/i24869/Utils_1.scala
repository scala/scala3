package demo

import scala.annotation.publicInBinary

class Utils {
  // Test case 1: Basic case class with @publicInBinary private constructor
  final case class TestClass @publicInBinary private () {}

  object TestClass {
    inline def apply(inline clips: List[String]): TestClass = new TestClass()
  }

  // Test case 2: Regular class with @publicInBinary private constructor
  class RegularClass @publicInBinary private (val x: Int) {
    def this() = this(0)
  }

  object RegularClass {
    inline def create(inline value: Int): RegularClass = new RegularClass(value)
  }

  // Test case 3: Nested class with @publicInBinary private constructor
  class Outer {
    class Inner @publicInBinary private (val name: String)
    object Inner {
      inline def make(inline n: String): Inner = new Inner(n)
    }
  }

  // Test case 4: @publicInBinary on private[Scope] method (not constructor)
  class WithPrivateMethod {
    @publicInBinary private[WithPrivateMethod] def secretMethod(x: Int): Int = x * 2
  }

  object WithPrivateMethod {
    inline def callSecret(w: WithPrivateMethod, inline x: Int): Int = w.secretMethod(x)
  }

  // Test case 5: @publicInBinary on private val accessor (via constructor)
  class WithPrivateVal @publicInBinary private (val hidden: String)

  object WithPrivateVal {
    inline def getHidden(w: WithPrivateVal): String = w.hidden
    inline def create(inline s: String): WithPrivateVal = new WithPrivateVal(s)
  }

  // Test case 6: Multiple @publicInBinary constructors with different arities
  class MultiConstructor @publicInBinary private (val a: Int, val b: String) {
    @publicInBinary private def this(a: Int) = this(a, "default")
  }

  object MultiConstructor {
    inline def make1(inline x: Int): MultiConstructor = new MultiConstructor(x)
    inline def make2(inline x: Int, inline y: String): MultiConstructor = new MultiConstructor(x, y)
  }

  // Test case 7: Case class with @publicInBinary and parameters
  final case class DataClass @publicInBinary private (name: String, value: Int)

  object DataClass {
    inline def make(inline n: String, inline v: Int): DataClass = new DataClass(n, v)
  }

  // Test case 8: @publicInBinary on private[Scope] val
  class WithPrivateValScoped {
    @publicInBinary private[WithPrivateValScoped] val secretVal: Int = 42
  }

  object WithPrivateValScoped {
    inline def getSecret(w: WithPrivateValScoped): Int = w.secretVal
  }

  // Test case 9: @publicInBinary on private[Scope] lazy val
  class WithLazyVal {
    @publicInBinary private[WithLazyVal] lazy val computed: String = "computed"
  }

  object WithLazyVal {
    inline def getComputed(w: WithLazyVal): String = w.computed
  }
}

// Test case 10: Top-level class with @publicInBinary (not nested in Utils)
class TopLevelPublicInBinary @publicInBinary private (val value: Double)

object TopLevelPublicInBinary {
  inline def create(inline v: Double): TopLevelPublicInBinary = new TopLevelPublicInBinary(v)
}

// Test case 11: Trait with companion having @publicInBinary class
trait Describable {
  def describe: String
}

class DescribableImpl @publicInBinary private (val desc: String) extends Describable {
  def describe: String = desc
}

object DescribableImpl {
  inline def create(inline d: String): Describable = new DescribableImpl(d)
}
