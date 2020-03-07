type L[X]
def foo = { class A; null.asInstanceOf[L[A]] }  // error // error
