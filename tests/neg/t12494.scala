
object X {
  // restriction in Scala 2 for local companions
  // restriction in Scala 3 under -from-tasty
  def m: Int = {
    trait C {
      protected[C] def f: Int
    }
    object C {
      class C2 extends C {
        protected[C] def f: Int = 42  // error // ok except for restrictions noted
        def test = f
      }
    }
    C.C2().test
  }
}
