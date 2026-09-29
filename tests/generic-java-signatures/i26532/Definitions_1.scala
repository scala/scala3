package i26532

trait Base[S, D]:
  final class Helper:
    def identity(value: S): S = value

  final def helper(): Helper = new Helper

abstract class Impl[S, D] extends Base[S, D]
