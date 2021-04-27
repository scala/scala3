trait A
class B extends A

transparent given g1: A = B()  // error: `transparent` can only be used in conjunction with `inline`

inline given g2: _ <: A:  // error: `=' expected // error
  def foo = 2
