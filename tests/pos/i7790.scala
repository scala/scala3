// https://github.com/scala/scala3/issues/7790
trait Foo:
  given Int = 10
  def map(f: Int ?=> Int) = f
  def map(f: Int ?=> String) = f

@main def Test =
  val m: Foo = ???
  m.map((x: Int) ?=> x)
