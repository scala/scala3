import language.experimental.captureChecking

class A[CS^]:
  self: A[CS^]^{CS} => // error: `^` on capture-set argument; meant A[{CS}] or A[CS]

class B[CS^]:
  self: B[{CS}]^{CS} => // ok: this is what was meant

def f[C^](b: B[{C}]): B[C^] = ??? // error

def g[D^](): Unit = ()
def h[C^](): Unit = g[C^]() // error
