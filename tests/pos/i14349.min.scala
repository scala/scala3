class Inv[M]

class Module:
  type X
  type Y
  type M = Module {
    type X = Module.this.X
    type Y = Module.this.Y
  }
  def expose = new Inv[M]
  def test                  = this match { case m => m.expose }
  // was: leak: `(m : Module)` in `m.expose: Inv[? <: Module { X = m.X }]`
  def res: Inv[_ <: Module] = this match { case m => m.expose }
