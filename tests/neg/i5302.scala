type L[X]
def foo = { class A; null.asInstanceOf[L[A]] }  // was error, now ok, since avoidance does not produce a bad type anymore
def bar(x: L[_]) = x // error
