import language.experimental.captureChecking

class Repro { self: Repro^ =>
  def coll: this.type = this

  def withFilter(p: Any^): WithFilter^{this, p} =
    new WithFilter(coll, p)
}

class WithFilter(val source: Repro^, val predicate: Any^)
