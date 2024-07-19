import language.experimental.captureChecking

trait A extends caps.Capability

object O:
  opaque type B = A