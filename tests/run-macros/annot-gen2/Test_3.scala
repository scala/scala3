class Bar:
  @foo def bar(s: String) = s

@main def Test =
  assert((new Bar).bar("bar") == "aahellohello")
