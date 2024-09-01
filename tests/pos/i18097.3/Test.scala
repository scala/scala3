package test

final case class Test(value: Opaque)

def test: Test =
  bar.go match
    case Some(value) => Test(value) // was error: Found: (value : Unit) Required: test.Opaque
    case _           => ???

def test2: Test =
  go(bar) match
    case Some(value) => Test(value)
    case _           => ???
