def fa(f: => Unit): Unit = ???
fa(42) // error
def test1 = fa(42)
def test2 = fa({42; ()})
