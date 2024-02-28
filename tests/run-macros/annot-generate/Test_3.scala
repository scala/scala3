//> using options -experimental -Yno-experimental

class Bar:
  @foo def bar(x: Int) = x + 1

@main def Test =
  assert((new Bar).bar(1) == 2)
