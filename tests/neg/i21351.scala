//> using options -Wconf:name=MissingEmptyArgumentList:e

def f = () => 42

object X:
  def apply() = 42

@main def Test =
  f.apply // error boosted warning
  X.apply // error always
