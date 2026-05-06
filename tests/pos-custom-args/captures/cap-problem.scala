import language.experimental.captureChecking

trait Suspend:
  type Suspension

  def resume(s: Suspension): Unit

import caps.SharedCapability

trait Async(val support: Suspend) extends SharedCapability

class CancelSuspension(ac: Async, suspension: ac.support.Suspension):
  ac.support.resume(suspension)
