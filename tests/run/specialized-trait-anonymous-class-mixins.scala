//> using options -language:experimental.specializedTraits

inline trait Foo[T: Specialized]:
  def stuff = "stuff"

trait Extra:
  def extra = "extra"

@main def Test =
  val foo = new Foo[Int]() with Extra {}
  assert(foo.extra + foo.stuff == "extrastuff")
  assert(foo.isInstanceOf[Extra])
