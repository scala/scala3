package gears.async

import language.experimental.captureChecking

import java.lang.invoke.{MethodHandles, VarHandle}
import java.util.concurrent.locks.ReentrantLock
import scala.annotation.unchecked.uncheckedVariance
import scala.concurrent.duration.FiniteDuration
import scala.annotation.constructorOnly
import scala.collection.mutable

object VThreadScheduler extends Scheduler:
  private val VTFactory = new java.util.concurrent.ThreadFactory:
    def newThread(r: Runnable): Thread =
      new Thread(null, r, "gears.async.VThread-", 0L)

  override def execute(body: Runnable): Unit =
    val th = VTFactory.newThread(body)
    th.start()
    ()

  private[gears] inline def unsafeExecute(body: Runnable^): Unit = execute(caps.unsafe.unsafeAssumePure(body))

  override def schedule(delay: FiniteDuration, body: Runnable): Cancellable =
    import caps.unsafe.unsafeAssumePure

    val sr = ScheduledRunnable(delay, body)
    // SAFETY: should not be able to access body, only for cancellation
    sr.unsafeAssumePure: Cancellable

  private final class ScheduledRunnable(delay: FiniteDuration, body: Runnable) extends Cancellable:
    @volatile var interruptGuard = true // to avoid interrupting the body

    val th = VTFactory.newThread: () =>
      try Thread.sleep(delay.toMillis)
      catch case e: InterruptedException => () /* we got cancelled, don't propagate */
      if ScheduledRunnable.interruptGuardVar.getAndSet(this, false) then body.run()
    th.start()

    final override def cancel(): Unit =
      if ScheduledRunnable.interruptGuardVar.getAndSet(this, false) then th.interrupt()
  end ScheduledRunnable

  private object ScheduledRunnable:
    val interruptGuardVar =
      MethodHandles
        .lookup()
        .in(classOf[ScheduledRunnable])
        .findVarHandle(classOf[ScheduledRunnable], "interruptGuard", classOf[Boolean])

object VThreadSupport extends AsyncSupport:
  type Scheduler = VThreadScheduler.type

  private final class VThreadLabel[R]() extends caps.Capability:
    private var result: Option[R] = None
    private val lock = ReentrantLock()
    private val cond = lock.newCondition()

    private[VThreadSupport] def clearResult() =
      lock.lock()
      result = None
      lock.unlock()

    private[VThreadSupport] def setResult(data: R) =
      lock.lock()
      try
        result = Some(data)
        cond.signalAll()
      finally lock.unlock()

    private[VThreadSupport] def waitResult(): R =
      lock.lock()
      try
        while result.isEmpty do cond.await()
        result.get
      finally lock.unlock()

  override opaque type Label[R, Cap^] <: caps.Capability = VThreadLabel[R]

  // outside boundary: waiting on label
  //  inside boundary: waiting on suspension
  private final class VThreadSuspension[-T, +R](using private[VThreadSupport] val l: VThreadLabel[R] @uncheckedVariance)
      extends gears.async.Suspension[T, R]:
    private var nextInput: Option[T] = None
    private val lock = ReentrantLock()
    private val cond = lock.newCondition()

    private[VThreadSupport] def setInput(data: T) =
      lock.lock()
      try
        nextInput = Some(data)
        cond.signalAll()
      finally lock.unlock()

    // variance is safe because the only caller created the object
    private[VThreadSupport] def waitInput(): T @uncheckedVariance =
      lock.lock()
      try
        while nextInput.isEmpty do cond.await()
        nextInput.get
      finally lock.unlock()

    // normal resume only tells other thread to run again -> resumeAsync may redirect here
    override def resume(arg: T): R =
      l.clearResult()
      setInput(arg)
      l.waitResult()

  override opaque type Suspension[-T, +R] <: gears.async.Suspension[T, R] = VThreadSuspension[T, R]

  override def boundary[R, Cap^](body: Label[R, Cap]^ ?->{Cap^} R): R =
    val label = VThreadLabel[R]()
    VThreadScheduler.unsafeExecute: () =>
      val result = body(using label)
      label.setResult(result)

    label.waitResult()

  override private[async] def resumeAsync[T, R](suspension: Suspension[T, R])(arg: T)(using Scheduler): Unit =
    suspension.l.clearResult()
    suspension.setInput(arg)

  override def scheduleBoundary[Cap^](body: Label[Unit, Cap] ?-> Unit)(using Scheduler): Unit =
    VThreadScheduler.execute: () =>
      val label = VThreadLabel[Unit]()
      body(using label)

  override def suspend[T, R, Cap^](body: Suspension[T, R]^{Cap^} => R^{Cap^})(using l: Label[R, Cap]^): T =
    val sus = new VThreadSuspension[T, R](using caps.unsafe.unsafeAssumePure(l))
    val res = body(sus)
    l.setResult(
      // SAFETY: will only be stored and returned by the Suspension resumption mechanism
      caps.unsafe.unsafeAssumePure(res)
    )
    sus.waitInput()
