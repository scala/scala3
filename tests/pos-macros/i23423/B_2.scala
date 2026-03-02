object Test:
  def test: Unit = pkg.Macro.foo
  // used to be error:
  // Found:    (hx: pkg.HasElem) => hx.Elem
  // Required: (he: pkg.HasElem) => he.Elem

