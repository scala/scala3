def f = () => 42

object X:
  def apply() = 42

@main def Test =
  f.apply // error
  X.apply // error
