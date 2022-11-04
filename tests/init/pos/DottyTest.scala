class ContextBase:
  class Inner:
    def foo(): ContextBase = ContextBase.this

class Outer:
  val ctx = new ContextBase {}
  println(ctx)
  val n = 10