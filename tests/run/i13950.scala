def example(x: Any & ([V] => V => Int)) =
  x[Int](1)
def example2(x: (Any & ([V] => V => Int)) @unchecked) =
  x[Int](1)
def example3[S <: Any & ([V] => V => Int)](x: S) =
  x[Int](1)

@main def Test =
  example([A] => (x: A) => 1)
  example2([A] => (x: A) => 1)
  example3([A] => (x: A) => 1)
