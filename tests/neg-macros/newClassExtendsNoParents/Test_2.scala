//> using options -experimental -Yno-experimental

def test: Any = makeClass("foo") // error
