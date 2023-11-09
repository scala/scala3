import language.experimental.captureChecking

trait Resource
def id[X](x: X): x.type = x
def foo[M <: Resource](r: M^): Unit = id(r)  // was error, should be ok
def bar[M](r: M^): Unit = id(r)  // ok
