//> using options -experimental -Yno-experimental
def test =
  transparentTestConflictingBounds // error
  transparentTestConflictingBoundsWithTypeLambda // error
  // testConflictingBounds // should throw an error here also, to be implemented before stabilisation
  // testConflictingBoundsWithTypeLambda // should throw an error here also, to be implemented before stabilisation
