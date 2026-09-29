//> using options -language:experimental.inlineTraits
inline trait Foo:
  val x = 10
  var y = 100
  def z = 5

trait Bar extends Foo

class Baz extends Bar

@main def Test = 
  val b = Baz()
  assert(b.y == 100)
  assert(b.x == 10)
  assert(b.z == 5)
