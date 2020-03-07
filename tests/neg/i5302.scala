type L[X]
def foo = { class A; null.asInstanceOf[L[A]] }  // error
def bar(x: L[_]) = x // error