//> using options -experimental
package example
// !! IMPORTANT: If you remove this test, also remove unroll-caseclass.check

import scala.annotation.unroll

// v3 of Unrolled
case class Unrolled(s: String, n: Int = 1, @unroll b: Boolean = true, @unroll l: Long = 0){
  def foo = s + n + b + l
}

// v3: ammendments to code that exercise a new parameter
object UnrollTestMainV3 extends TestUtil {
  def main(args: Array[String]): Unit = {
    UnrollTestScalaSpecificV3()
    UnrollTestPlatformSpecificV3()

    logAssertStartsWith(new Unrolled("cow").foo, "cow1true0")
    logAssertStartsWith(new Unrolled("cow", 2).foo, "cow2true0")
    logAssertStartsWith(new Unrolled("cow", 2, false).foo, "cow2false0")
    logAssertStartsWith(new Unrolled("cow", 2, false, 9L).foo, "cow2false9")

    logAssertStartsWith(Unrolled("cow").foo, "cow1true0")
    logAssertStartsWith(Unrolled("cow", 2).foo, "cow2true0")
    logAssertStartsWith(Unrolled("cow", 2, false).foo, "cow2false0")
    logAssertStartsWith(Unrolled("cow", 2, false, 9L).foo, "cow2false9")

    val unrolled = Unrolled("cow")

    logAssertStartsWith(unrolled.copy(s = "cow").foo, "cow1true0")
    logAssertStartsWith(unrolled.copy(s = "cow", n = 2).foo, "cow2true0")
    logAssertStartsWith(unrolled.copy(s = "cow", n = 2, b = false).foo, "cow2false0")
    logAssertStartsWith(unrolled.copy(s = "cow", n = 2, b = false, l = 9L).foo, "cow2false9")

    val Unrolled(s, n, b, l) = unrolled

    assert(s == "cow")
    assert(n == 1)
    assert(b == true)
    assert(l == 0L)


  }
}

object UnrollTestScalaSpecificV3 extends TestUtil {
  def apply() = {
    val unrolled = summon[scala.deriving.Mirror.Of[Unrolled]].fromProduct(
      new Product {
        def canEqual(that: Any) = true
        def productArity = 4
        def productElement(n: Int) = n match {
          case 0 => "hello"
          case 1 => 31337
          case 2 => false
          case 3 => 12345L
        }
      }
    )

    logAssertStartsWith(unrolled.foo, "hello31337false12345")
  }
}

object UnrollTestPlatformSpecificV3 extends TestUtil {
  def apply() = {
    val cls = classOf[Unrolled]

    assert(scala.util.Try(cls.getConstructor(classOf[String])).isFailure)
    println("as expected, no constructor for Unrolled(s: String)")
    assert(
      cls.getConstructor(classOf[String], classOf[Int])
        .newInstance("hello", 2: Integer)
        .asInstanceOf[Unrolled]
        .foo ==
        "hello2true0"
    )
    assert(
      cls.getConstructor(classOf[String], classOf[Int], classOf[Boolean])
        .newInstance("hello", 2: Integer, java.lang.Boolean.FALSE)
        .asInstanceOf[Unrolled]
        .foo ==
        "hello2false0"
    )
    assert(
      cls.getConstructor(classOf[String], classOf[Int], classOf[Boolean], classOf[Long])
        .newInstance("hello", 2: Integer, java.lang.Boolean.FALSE, 3: Integer)
        .asInstanceOf[Unrolled]
        .foo ==
        "hello2false3"
    )

    cls.getConstructors.foreach(println)
  }
}
