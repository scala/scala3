// testNotNull can be inserted during PatternMatcher
def f(xs: List[String]) =
  xs.zipWithIndex.collect {
    case (arg, idx) => idx
  }