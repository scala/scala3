import language.experimental.captureChecking
import caps.assumeSafe

@assumeSafe
object A:
  // get @uncheckedVariance on the types
  def f(
      a: List[String] = List.empty,
      b: Option[String] = None,
      c: Long = 30000
    ) = ()
