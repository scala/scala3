import language.experimental.captureChecking

trait A:
  type T <: caps.Capability

class B extends A:
  type T = C

class C extends caps.SharedCapability


trait A2:
  type T[Cap^]

  def takesCap[Cap^](t: T[Cap]): Unit

