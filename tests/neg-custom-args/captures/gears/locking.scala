/** Package listeners provide some auxilliary methods to work with listeners. */
package gears.async.listeners

import language.experimental.captureChecking

import gears.async._

import scala.annotation.tailrec

import Listener.ListenerLock

/** Two listeners being locked at the same time, while having the same [[Listener.ListenerLock.selfNumber lock number]].
  */
case class ConflictingLocksException(
    listeners: (Listener[?]^, Listener[?]^)
) extends Exception

/** Attempt to lock both listeners belonging to possibly different sources at the same time. Lock orders are respected
  * by comparing numbers on every step.
  *
  * Returns `true` on success, or the listener that fails first.
  *
  * @throws ConflictingLocksException
  *   In the case that two locks sharing the same number is encountered, this exception is thrown with the conflicting
  *   listeners.
  */
def lockBoth[T, U](
    lt: Listener[T]^,
    lu: Listener[U]^
): (lt.type | lu.type | true) =
  val lockT = if lt.lock == null then return (if lu.acquireLock() then true else lu) else lt.lock
  val lockU = if lu.lock == null then return (if lt.acquireLock() then true else lt) else lu.lock

  def doLock[T, U](lt: Listener[T]^, lu: Listener[U]^)(
      lockT: ListenerLock^{lt},
      lockU: ListenerLock^{lu}
  ): (lt.type | lu.type | true) =
    // assert(lockT.number > lockU.number)
    if !lockT.acquire() then lt
    else if !lockU.acquire() then
      lockT.release()
      lu
    else true

  if lockT.selfNumber == lockU.selfNumber then throw ConflictingLocksException((lt, lu))
  else if lockT.selfNumber > lockU.selfNumber then doLock(lt, lu)(lockT, lockU)
  else doLock(lu, lt)(lockU, lockT)
end lockBoth
