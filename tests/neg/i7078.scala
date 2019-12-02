trait A
class B extends A

given g1 <: A = B()  // error: `<:' is only allowed for given with `inline' modifier // error

inline given g2 <: A  // error: <: A is not a class type
  def foo = 2    // error: `=' expected

