package demo

object Demo extends Utils {
  // Test case 1: Basic case class with @publicInBinary private constructor
  val test1 = TestClass(???)

  // Test case 2: Regular class with @publicInBinary private constructor
  val test2 = RegularClass.create(42)

  // Test case 3: Nested class with @publicInBinary private constructor
  val outer = new Outer
  val test3 = outer.Inner.make("hello")

  // Test case 4: @publicInBinary on private[Scope] method
  val wpm = new WithPrivateMethod
  val test4 = WithPrivateMethod.callSecret(wpm, 21)

  // Test case 5: @publicInBinary on private val accessor (via constructor)
  val test5a = WithPrivateVal.create("secret")
  val test5b = WithPrivateVal.getHidden(test5a)

  // Test case 6: Multiple @publicInBinary constructors
  val test6a = MultiConstructor.make1(100)
  val test6b = MultiConstructor.make2(200, "custom")

  // Test case 7: Case class with @publicInBinary and parameters
  val test7 = DataClass.make("test", 123)

  // Test case 8: @publicInBinary on private[Scope] val
  val wpvs = new WithPrivateValScoped
  val test8 = WithPrivateValScoped.getSecret(wpvs)

  // Test case 9: @publicInBinary on private[Scope] lazy val
  val wlv = new WithLazyVal
  val test9 = WithLazyVal.getComputed(wlv)

  // Test case 10: Top-level class with @publicInBinary
  val test10 = TopLevelPublicInBinary.create(3.14)

  // Test case 11: Trait with companion having @publicInBinary class
  val test11: Describable = DescribableImpl.create("description")
}
