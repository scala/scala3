import language.experimental.captureChecking
class Ref extends caps.Mutable
def f1[C1^](a: Ref^{C1}): Unit =
  val t = a
