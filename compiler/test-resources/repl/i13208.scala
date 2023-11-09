//> using options -source:future -deprecation
scala> type M[X] = X match { case Int => String case _ => Int }
scala> type N[X] = X match { case List[_] => Int }
