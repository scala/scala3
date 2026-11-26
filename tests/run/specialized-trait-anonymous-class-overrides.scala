//> using options -language:experimental.specializedTraits

inline trait Foo[T: Specialized](value: T):
  def getValue: T = value

@main def Test =
  val foo: Foo[Int] = new Foo[Int](7) { override def getValue = 8 }
  assert(foo.getValue == 8)
