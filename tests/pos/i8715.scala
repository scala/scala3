@main
def Test = List(42) match { case List(xs @ (ys*)) => xs }
