package tests
package matchTypeTuple

// issue 16084

sealed trait TupleTest[Take[_, _], Drop[_, _]]:
  type Split[T <: Tuple, N <: Int] = (Take[T, N], Drop[T, N])

  inline def splitAt[This <: Tuple](n: Int): Split[This, n.type]
    = ???
