//> using options -language:experimental.specializedTraits

inline trait Foo[T: Specialized](value: T):
  def getValue: T = value

@main def Test =
  val k = 5
  val foo: Foo[Int] = new Foo[Int](42) { def bar = k + 1; override def getValue = bar }
  assert(foo.getValue == 6)
