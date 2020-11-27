@main
def Test = List(42) match { case List((ys: _*) as xs) => xs }  // error
