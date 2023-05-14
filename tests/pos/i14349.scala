trait Module:
  self =>
  type M <: Module {
    type X = self.X
    type Y = self.Y
  }
  type X
  type Y

  def expose: Expose[X, Y, M]

trait Expose[
    X0,
    Y0,
    M <: Module { type X = X0; type Y = Y0 }
]

def test(ms: Seq[Option[Module]]): Seq[Expose[_, _, _]] =
  ms.collect { case Some(module) => module.expose }
