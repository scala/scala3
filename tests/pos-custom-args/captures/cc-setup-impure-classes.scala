import language.experimental.captureChecking

trait Resource
def id[X](x: X): x.type = x
def foo[M <: Resource](r: M^): Unit = id(r)
