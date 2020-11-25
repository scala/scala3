trait A
class B extends A

transparent given A as g1 = B()  // error: `transparent` can only be used in conjunction with `inline`

inline given g2 as _ <: A:  // error: `=' expected
  def foo = 2
