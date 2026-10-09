def test =
  Impl.CONST // ok: static interface fields are inherited
  Impl.m() // error: static interface methods are not

def testImport =
  import Impl.*
  CONST
  m() // error

// member classes are not inherited
def testNestedType: Impl.Nested = ??? // error
