class A

type B = A

def f[T](x: T): x.type & T = ???

def g = {
  var a: B = ???
  f[A](a)
}

def testNN = {
  var s: String = ???
  s.nn
}
