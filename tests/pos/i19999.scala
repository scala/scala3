class InIntersection[I, A]

def derived[A, R0]: InIntersection[A & R0, A] = new InIntersection[A & R0, A]

var x: InIntersection[Int & String, Int] = derived

