trait TyperCrasher {
  class CrashTyper(i: Int) {}
  object CrashTyper {
    def init: CrashTyper = CrashTyper(0)
  }
}
