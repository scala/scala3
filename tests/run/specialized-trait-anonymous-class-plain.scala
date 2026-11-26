// scalajs: --skip
//> using options -language:experimental.specializedTraits

inline trait Foo[T: Specialized](value: T):
  def getValue: T = value

@main def Test =
  // A simple instance is replaced by the shared $impl$ class
  val f1 = new Foo[Int](1) {}
  val f2 = new Foo[Int](2) {}
  assert(f1.getValue == 1 && f2.getValue == 2)
  assert(f1.getClass eq f2.getClass)
  assert(f1.getClass.getName == "Foo$impl$scala$Int")

  // while an anonymous class with members is not
  val f3 = new Foo[Int](3) { def bar = 1 }
  assert(f3.getClass ne f1.getClass)
