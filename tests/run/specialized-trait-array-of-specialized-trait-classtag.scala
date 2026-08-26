//> using options -language:experimental.specializedTraits

inline trait Foo[T: Specialized]:
  val x = 10

@main def Test = 
  val xs: Array[Foo[Int]] = Array.tabulate(10)(_ => new Foo[Int] {})
  println(xs.head.x)
