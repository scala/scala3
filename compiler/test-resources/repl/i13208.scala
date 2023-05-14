// scalac: -source:future -deprecation
scala> type M[X] = X match { case Int => String case _ => Int }
scala> type N[X] = X match { case List[_] => Int }
1 warning found
-- Deprecation Warning: --------------------------------------------------------
1 | type N[X] = X match { case List[_] => Int }
  |                                 ^
  |        `_` is deprecated for wildcard arguments of types: use `?` instead
