trait A
class B extends A

given g1 as _ <: A = B()  // error: `_ <:' is only allowed for given with `inline' modifier // error

inline given g2 as _ <: A:  // error: `=' expected
  def foo = 2
// error