// This tests language features that have a special handling in the Scala 2 library TASTy.
// These should behave the same way from if compiled against the Scala 2 library class files ot TASTy.

import scala.util.*
import scala.compiletime.testing.typeChecks

enum Color:
  case Red, Green, Blue

object Test:
  def main(args: Array[String]): Unit = {
    println("hello 2.13 library bootstrapped")
    println(Color.Red)
    println(Color.Green)
    println(Color.Blue)

    testScala2UnapplySignatures()
    testScala2ObjectParents()
    testScala2CaseClassUnderscoreMembers()
    testScalaNumberUnderlying()
    testArrayOps()
    scala.collection.mutable.UnrolledBufferTest.test()
  }

  def testScala2UnapplySignatures() = {
    val _: Option[Int] = Some.unapply(Some(1))
    val _: Option[Int] = Right.unapply(Right(1))
    val _: Option[(Int, List[Int])] = ::.unapply(::(1, Nil))

    val _: Option[Int] = Tuple1.unapply(Tuple1(1))
    val _: Option[(Int, Int)] = Tuple2.unapply((1, 2))
    val _: Option[(Int, Int, Int)] = Tuple3.unapply((1, 2, 3))
  }

  def testScala2ObjectParents() = {
    assert(!typeChecks("Either: scala.deriving.Mirror.Sum"))
    assert(!typeChecks("Either: scala.deriving.Mirror"))
  }

  def testScala2CaseClassUnderscoreMembers() = {
    val some: Some[Int] = Some(1)
    assert(!typeChecks("some._1"))
  }

  def testScalaNumberUnderlying() = {
    import scala.math.{ScalaNumericConversions, ScalaNumber}

    val _: java.math.BigInteger = BigInt(1).underlying
    val _: Object = (BigInt(1): ScalaNumericConversions).underlying
    val _: Object = (BigInt(1): ScalaNumber).underlying

    // val _: java.math.BigDecimal = BigDecimal(1).underlying // FIXME: inferred result type of non-private method
    val _: Object = (BigDecimal(1): ScalaNumericConversions).underlying
    val _: Object = (BigDecimal(1): ScalaNumber).underlying

    class MyNumber1(override val underlying: BigInt) extends ScalaNumericConversions {
      def doubleValue: Double = ???; def floatValue: Float = ???;
      def intValue: Int = ???; def longValue: Long = ???
      def isWhole: Boolean = ???
    }
    val _: BigInt = MyNumber1(1).underlying
    val _: Object = (MyNumber1(1): ScalaNumericConversions).underlying
    val _: Object = (MyNumber1(1): ScalaNumber).underlying

    class MyNumber2(override val underlying: Object) extends ScalaNumber {
      def doubleValue: Double = ???; def floatValue: Float = ???;
      def intValue: Int = ???; def longValue: Long = ???
      def isWhole: Boolean = ???
    }
    val _: Object = MyNumber2(BigInt(1)).underlying
    val _: Object = (MyNumber2(BigInt(1)): ScalaNumber).underlying
  }

  def testArrayOps() = {
    new collection.ArrayOps[String](Array[String]("foo")).exists(x => true)
  }

end Test
