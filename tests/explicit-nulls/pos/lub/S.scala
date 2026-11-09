// Minimized from https://scala3.westeurope.cloudapp.azure.com/dashboard/projects/greenfossil/thorium/builds/HarrisL2%2Fscala3%3Aunsafe-explicit-nulls%3A2026-04-01/logs

class A
class B

def foo(fn: A => A | B): A | B =
  val v: J[A] = ???
  val resp = v.execute(fn(_), ???)
  resp