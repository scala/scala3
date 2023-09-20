//> using options -experimental

@main
def run =
  T.run[Int]
  T.run[C]
  T.run[List]
  T.run[Map]

class C[T <: Int]
