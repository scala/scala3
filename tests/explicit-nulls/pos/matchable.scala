def foo1[T <: Matchable](t: T) = t match { case t: Null => () }

def foo2[T <: Matchable](t: T) = t match { case null => () }