import language.experimental.captureChecking

// Inferring `C` adds `C2` to the union {C1, C} of `Holder`'s second parameter,
// which forwards it to `C`; the bound `C <: {}` of the first parameter rejects it.
// The rejection must be undone in the union too. Otherwise the union keeps `C2`,
// `C` is solved to {} and the call is accepted, although `h.get` captures `C2`.
def boom[C1^, C2^](op: () ->{C1, C2} Unit): () ->{C1} Unit =
  class Holder[C^](k: (() ->{C} Unit) => Unit, g: () ->{C1, C} Unit):
    def get: () ->{C1, C} Unit = g
  def pure(g: () -> Unit): Unit = ()
  val h = Holder(g => pure(g), op)  // error
  h.get
