package dotty.tools.debug

import com.sun.jdi.*
import com.sun.jdi.event.*
import com.sun.jdi.request.*

import java.lang.ref.Reference
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*
import java.util.concurrent.TimeoutException

class Debugger(vm: VirtualMachine, maxDuration: Duration, verbose: Boolean = false):
  // For some JDI events that we receive, we wait for client actions.
  // Example: On a BreakpointEvent, the client may want to inspect frames and variables, before it
  // decides to step in or continue.
  private val pendingEvents = new LinkedBlockingQueue[Event]()

  // Internal event subscriptions, to react to JDI events
  // Example: add a Breakpoint on a ClassPrepareEvent
  private val eventSubs = new AtomicReference(List.empty[PartialFunction[Event, Unit]])
  private val eventListener = startListeningVM()

  def configureBreakpoint(className: String, line: Int): Unit =
    vm.classesByName(className).asScala.foreach(addBreakpoint(_, line))
    // watch class preparation and add breakpoint when the class is prepared
    val request = vm.eventRequestManager.createClassPrepareRequest
    request.addClassFilter(className)
    subscribe:
      case e: ClassPrepareEvent if e.request == request => addBreakpoint(e.referenceType, line)
    request.enable()

  def break(): ThreadReference = receiveEvent { case e: BreakpointEvent => e.thread }

  def continue(thread: ThreadReference): Unit = thread.resume()

  def next(thread: ThreadReference): ThreadReference =
    stepAndWait(thread, StepRequest.STEP_LINE, StepRequest.STEP_OVER)

  def step(thread: ThreadReference): ThreadReference =
    stepAndWait(thread, StepRequest.STEP_LINE, StepRequest.STEP_INTO)

  /** stop listening and disconnect debugger */
  def dispose(): Unit =
    eventListener.interrupt()
    vm.dispose()

  private def addBreakpoint(refType: ReferenceType, line: Int): Unit =
    try
      for location <- refType.locationsOfLine(line).asScala do
        if verbose then println(s"Adding breakpoint in $location")
        val breakpoint = vm.eventRequestManager.createBreakpointRequest(location)
        // suspend only the thread which triggered the event
        breakpoint.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
        // let's enable the breakpoint and forget about it
        // we don't need to store it because we never remove any breakpoint
        breakpoint.enable()
    catch
      case e: AbsentInformationException =>
        if verbose then println(s"AbsentInformationException on ${refType}")

  private def stepAndWait(thread: ThreadReference, size: Int, depth: Int): ThreadReference =
    val request = vm.eventRequestManager.createStepRequest(thread, size, depth)
    request.enable()
    thread.resume()
    // Because our debuggee is mono-threaded, we don't check that `e.request` is our step request.
    // Indeed there can be only one step request per thread at a time.
    val newThreadRef = receiveEvent { case e: StepEvent => e.thread }
    request.disable()
    newThreadRef

  private def subscribe(f: PartialFunction[Event, Unit]): Unit =
    eventSubs.updateAndGet(f :: _)

  private def startListeningVM(): Thread =
    val thread = Thread: () =>
      var isAlive = true
      try while isAlive do
        val eventSet = vm.eventQueue.remove()
        val subscriptions = eventSubs.get
        var shouldResume = true
        for event <- eventSet.iterator.asScala.toSeq do
          if verbose then println(formatEvent(event))
          for f <- subscriptions if f.isDefinedAt(event) do f(event)
          event match
            case e: (BreakpointEvent | StepEvent) =>
              shouldResume = false
              pendingEvents.put(e)
            case _: VMDisconnectEvent => isAlive = false
            case _ => ()
        if shouldResume then eventSet.resume()
      catch case _: InterruptedException => ()
    thread.start()
    thread
  end startListeningVM

  private def receiveEvent[T](f: PartialFunction[Event, T]): T =
    // poll repeatedly until we get an event that matches the partial function or a timeout
    Iterator.continually(pendingEvents.poll(maxDuration.toMillis, TimeUnit.MILLISECONDS))
      .map(e => if (e == null) throw new TimeoutException() else e)
      .collect(f)
      .next()

  private def formatEvent(event: Event): String =
    event match
      case e: ClassPrepareEvent => s"$e ${e.referenceType}"
      case e => e.toString

object Debugger:
  // The socket JDI connector
  private val connector = Bootstrap.virtualMachineManager
    .attachingConnectors
    .asScala
    .find(_.getClass.getName == "com.sun.tools.jdi.SocketAttachingConnector")
    .get

  def apply(jdiPort: Int, maxDuration: Duration): Debugger =
    val arguments = connector.defaultArguments()
    arguments.get("hostname").setValue("localhost")
    arguments.get("port").setValue(jdiPort.toString)
    arguments.get("timeout").setValue(maxDuration.toMillis.toString)
    val vm = connector.attach(arguments)
    new Debugger(vm, maxDuration)

