import language.experimental.captureChecking

trait Suspend:
  type Suspension

  def resume(s: Suspension): Unit

import caps.Capability

trait Async(val support: Suspend) extends Capability

class CancelSuspension(ac: Async, suspension: ac.support.Suspension):
  ac.support.resume(suspension)
