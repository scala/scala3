//> using options -source future -language:experimental.modularity
class C(tracked x: Int) // error

class C2(tracked var x: Int) // error

object A:
  def foo(tracked a: Int) = // error
    tracked val b: Int = 2 // error

object B:
  tracked object Foo // error

object C:
  tracked class D // error

object D:
  tracked type T = Int // error

object E:
  given g2: (tracked val x: Int) => C = C(x) // error
