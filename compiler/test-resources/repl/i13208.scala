//> using options -language:`3.4-migration` -deprecation
scala> type M[X] = X match { case Int => String case _ => Int }
scala> type N[X] = X match { case List[_] => Int }
-- Error: ----------------------------------------------------------------------
1 | type N[X] = X match { case List[_] => Int }
  |                                 ^
  |        `_` is deprecated for wildcard arguments of types: use `?` instead
