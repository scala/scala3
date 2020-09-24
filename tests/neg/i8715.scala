@main
def Test = List(42) match { case List(xs as (ys: _*)) => xs }  // error
