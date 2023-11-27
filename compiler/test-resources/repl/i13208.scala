//> using options -source:3.4-migration
scala> type M[X] = X match { case Int => String case _ => Int }
scala> type N[X] = X match { case List[_] => Int }
