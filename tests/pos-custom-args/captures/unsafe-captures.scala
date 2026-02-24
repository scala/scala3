import annotation.unchecked.uncheckedCaptures
import caps.unsafe.untrackedCaptures

class LL[+A] private (@untrackedCaptures private var lazyState: (() => LL.State[A]^) @uncheckedCaptures):
  private val res = lazyState() // without unchecked captures we get a van't unbox any error


object LL:

  private trait State[+A]
