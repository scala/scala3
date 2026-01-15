import language.experimental.captureChecking

trait A extends caps.SharedCapability

object O:
  opaque type B = A