// scalac: -source:future -deprecation
scala> type M[X] = X match { case Int => String case _ => Int }
-- Deprecation Warning:
1 | type M[X] = X match { case Int => String case _ => Int }
  |                                               ^
  |        `_` is deprecated for wildcard arguments of types: use `?` instead
scala> type N[X] = X match { case List[_] => Int }
-- Deprecation Warning:
1 | type N[X] = X match { case List[_] => Int }
  |                                 ^
  |        `_` is deprecated for wildcard arguments of types: use `?` instead
