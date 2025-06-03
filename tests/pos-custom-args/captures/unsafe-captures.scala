import annotation.unchecked.uncheckedCaptures
class LL[+A] private (private var lazyState: (() => LL.State[A]^) @uncheckedCaptures):
  private val res = lazyState() // without unchecked captures we get a van't unbox cap error


object LL:

  private trait State[+A]
