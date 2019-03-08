// These methods are not yet on Product.scala (added in 2.13.x)
trait Product2_13 extends Product {
  def productElementName(n: Int): String

  /** An iterator over the names of all the elements of this product.
    */
  def productElementNames: Iterator[String] = new scala.collection.AbstractIterator[String] {
    private[this] var c: Int = 0
    private[this] val cmax = productArity

    def hasNext = c < cmax

    def next() = {
      val result = productElementName(c); c += 1; result
    }
  }
}

case class User(name: String, age: Int) extends Product2_13

case class ユーザー(名前: String, 年齢: Int) extends Product2_13

case class U$er(na$me: String, a$ge: Int) extends Product2_13

case class `type`(`for`: String, `if`: Int) extends Product2_13

case class `contains spaces`(`first param`: String, `second param`: Int) extends Product2_13

case class Symbols(:: : String, || : Int) extends Product2_13

case class MultipleParamLists(a: String, b: Int)(c: Boolean) extends Product2_13

case class AuxiliaryConstructor(a: String, b: Int) extends Product2_13 {
  def this(x: String) = {
    this(x, 123)
  }
}

case class OverloadedApply(a: String, b: Int) extends Product2_13
object OverloadedApply {
  def apply(x: String): OverloadedApply =
    new OverloadedApply(x, 123)
}

case class NoParams() extends Product2_13

//case class DefinesProductElementName(a: String, b: Int)  extends Product2_13 {
//  override def productElementName(n: Int): String = "foo"
//}

//trait A {
//  override def productElementName(n: Int): String = "overriden"
//}
//case class InheritsProductElementName(a: String, b: Int) extends A
//
//trait B extends Product2_13 {
//  override def productElementName(n: Int): String = "overriden"
//}
//case class InheritsProductElementName_Override(a: String, b: Int) extends B
//
//trait C { self: Product =>
//  override def productElementName(n: Int): String = "overriden"
//}
//case class InheritsProductElementName_Override_SelfType(a: String, b: Int) extends C

case class PrivateMembers(a: Int, private val b: Int, c: Int, private val d: Int, e: Int, private val f: Int) extends Product2_13

object Test extends App {
  def pretty(p: Product2_13): String =
    p.productElementNames.zip(p.productIterator)
      .map { case (name, value) => s"$name=$value" }
      .mkString(p.productPrefix + "(", ", ", ")")

  println(pretty(User("Susan", 42)))
  println(pretty(ユーザー("Susan", 42)))
  println(pretty(U$er("Susan", 42)))
  println(pretty(`type`("Susan", 42)))
  println(pretty(`contains spaces`("Susan", 42)))
  println(pretty(Symbols("Susan", 42)))
  println(pretty(MultipleParamLists("Susan", 42)(true)))
  println(pretty(AuxiliaryConstructor("Susan", 42)))
  println(pretty(OverloadedApply("Susan")))
//  println(pretty(DefinesProductElementName("Susan", 42)))

//  // uses the synthetic, not the one defined in the trait
//  println(pretty(InheritsProductElementName("Susan", 42)))
//
//  // uses the override defined in the trait
//  println(pretty(InheritsProductElementName_Override("Susan", 42)))
//
//  // uses the synthetic, not the one defined in the trait
//  println(pretty(InheritsProductElementName_Override_SelfType("Susan", 42)))

  println(pretty(PrivateMembers(10, 20, 30, 40, 50, 60)))
  println(pretty(NoParams()))
}
