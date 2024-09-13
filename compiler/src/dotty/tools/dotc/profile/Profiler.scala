package dotty.tools.dotc.profile

import scala.annotation.*
import scala.language.unsafeNulls

import java.io.{FileWriter, PrintWriter}
import java.nio.file.Paths
import java.lang.management.{ManagementFactory, GarbageCollectorMXBean, RuntimeMXBean, MemoryMXBean, ClassLoadingMXBean, CompilationMXBean}
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import javax.management.openmbean.CompositeData
import javax.management.{Notification, NotificationEmitter, NotificationListener}

import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Symbols.{Symbol, NoSymbol}
import dotty.tools.dotc.core.Flags
import dotty.tools.backend.jvm.DottyBackendInterface.symExtensions
import dotty.tools.io.AbstractFile
import annotation.internal.sharable
import dotty.tools.dotc.core.Periods.InitialRunId
import scala.collection.mutable.UnrolledBuffer

object Profiler {
  def apply()(using Context): Profiler =
    if (!ctx.settings.YprofileEnabled.value) NoOpProfiler
    else {
      val reporter = if (ctx.settings.YprofileDestination.value != "")
        new StreamProfileReporter(new PrintWriter(new FileWriter(ctx.settings.YprofileDestination.value, true)))
      else ConsoleProfileReporter
      new RealProfiler(reporter)
    }

  final def NoOp: Profiler = NoOpProfiler

  private[profile] val emptySnap: ProfileSnap = ProfileSnap(0, "", 0, 0, 0, 0, 0, 0, 0, 0)
}

case class GcEventData(pool:String, reportTimeNs: Long, gcStartMillis:Long, gcEndMillis:Long, durationMillis: Long, name:String, action:String, cause:String, threads:Long){
  val endNanos = System.nanoTime()
}

case class ProfileSnap(threadId: Long, threadName: String, snapTimeNanos : Long,
                       idleTimeNanos:Long, cpuTimeNanos: Long, userTimeNanos: Long,
                       allocatedBytes:Long, heapBytes:Long,
                       totalClassesLoaded: Long, totalJITCompilationTime: Long) {
  def updateHeap(heapBytes:Long): ProfileSnap =
    copy(heapBytes = heapBytes)
}
case class ProfileRange(start: ProfileSnap, end:ProfileSnap, phase:Phase, purpose:String, taskCount:Int, thread:Thread) {
  def allocatedBytes: Long = end.allocatedBytes - start.allocatedBytes

  def userNs: Long = end.userTimeNanos - start.userTimeNanos

  def cpuNs: Long = end.cpuTimeNanos - start.cpuTimeNanos

  def idleNs: Long = end.idleTimeNanos - start.idleTimeNanos

  def runNs: Long = end.snapTimeNanos - start.snapTimeNanos


  private def toMillis(ns: Long) = ns / 1000000.0D

  private def toMegaBytes(bytes: Long) = bytes / 1000000.0D


  def wallClockTimeMillis: Double = toMillis(end.snapTimeNanos - start.snapTimeNanos)

  def idleTimeMillis: Double = toMillis(end.idleTimeNanos - start.idleTimeNanos)

  def cpuTimeMillis: Double = toMillis(end.cpuTimeNanos - start.cpuTimeNanos)

  def userTimeMillis: Double = toMillis(end.userTimeNanos - start.userTimeNanos)

  def allocatedMB: Double = toMegaBytes(end.allocatedBytes - start.allocatedBytes)

  def retainedHeapMB: Double = toMegaBytes(end.heapBytes - start.heapBytes)
}

private opaque type TracedEventId <: String = String
private object TracedEventId:
  def apply(stringValue: String): TracedEventId = stringValue
  final val Empty: TracedEventId = ""

sealed trait Profiler {

  def finished(): Unit

  inline def onPhase[T](phase: Phase)(inline body: T): T =
    val (event, snapshot) = beforePhase(phase)
    try body
    finally afterPhase(event, phase, snapshot)
  protected final val EmptyPhaseEvent = (TracedEventId.Empty, Profiler.emptySnap)
  protected def beforePhase(phase: Phase): (TracedEventId, ProfileSnap) = EmptyPhaseEvent
  protected def afterPhase(event: TracedEventId, phase: Phase, profileBefore: ProfileSnap): Unit = ()

  inline def onUnit[T](phase: Phase, unit: CompilationUnit)(inline body: T): T =
    val event = beforeUnit(phase, unit)
    try body
    finally afterUnit(event)
  protected def beforeUnit(phase: Phase, unit: CompilationUnit): TracedEventId = TracedEventId.Empty
  protected def afterUnit(event: TracedEventId): Unit = ()

  inline def onTypedDef[T](sym: Symbol)(inline body: T): T =
    val event = beforeTypedDef(sym)
    try body
    finally afterTypedDef(event)
   protected def beforeTypedDef(sym: Symbol): TracedEventId = TracedEventId.Empty
   protected def afterTypedDef(token: TracedEventId): Unit = ()

  inline def onImplicitSearch[T](pt: Type)(inline body: T): T =
    val event = beforeImplicitSearch(pt)
    try body
    finally afterImplicitSearch(event)
  protected def beforeImplicitSearch(pt: Type): TracedEventId  = TracedEventId.Empty
  protected def afterImplicitSearch(event: TracedEventId): Unit = ()

  inline def onMacroSplice[T](macroSym: Symbol)(inline body: T): T =
    val event = beforeMacroSplice(macroSym)
    try body
    finally afterMacroSplice(event)
  protected def beforeMacroSplice(macroSym: Symbol): TracedEventId = TracedEventId.Empty
  protected def afterMacroSplice(event: TracedEventId): Unit = ()

  inline def onCompletion[T](root: Symbol, associatedFile: => AbstractFile)(inline body: T): T =
    val (event, completionName) = beforeCompletion(root, associatedFile)
    try body
    finally afterCompletion(event, completionName)
  protected final val EmptyCompletionEvent = (TracedEventId.Empty, "")
  protected def beforeCompletion(root: Symbol, associatedFile: => AbstractFile): (TracedEventId, String) = EmptyCompletionEvent
  protected def afterCompletion(event: TracedEventId, completionName: String): Unit = ()
}
private [profile] object NoOpProfiler extends Profiler {
  override def finished(): Unit = ()
}

private [profile] object RealProfiler {
  import scala.jdk.CollectionConverters.*
  val runtimeMx: RuntimeMXBean = ManagementFactory.getRuntimeMXBean
  val memoryMx: MemoryMXBean = ManagementFactory.getMemoryMXBean
  val gcMx: List[GarbageCollectorMXBean] = ManagementFactory.getGarbageCollectorMXBeans.asScala.toList
  val classLoaderMx: ClassLoadingMXBean = ManagementFactory.getClassLoadingMXBean
  val compileMx: CompilationMXBean = ManagementFactory.getCompilationMXBean
  val threadMx: ExtendedThreadMxBean = ExtendedThreadMxBean.proxy
  if (threadMx.isThreadCpuTimeSupported) threadMx.setThreadCpuTimeEnabled(true)
  private val idGen = new AtomicInteger()

  @nowarn("cat=deprecation")
  private[profile] def snapThread(idleTimeNanos: Long): ProfileSnap = {
    import RealProfiler.*
    val current = Thread.currentThread()

    ProfileSnap(
      threadId = current.getId,
      threadName = current.getName,
      snapTimeNanos = System.nanoTime(),
      idleTimeNanos = idleTimeNanos,
      cpuTimeNanos = threadMx.getCurrentThreadCpuTime,
      userTimeNanos = threadMx.getCurrentThreadUserTime,
      allocatedBytes = threadMx.getThreadAllocatedBytes(Thread.currentThread().getId),
      heapBytes = readHeapUsage(),
      totalClassesLoaded = classLoaderMx.getTotalLoadedClassCount,
      totalJITCompilationTime = compileMx.getTotalCompilationTime
    )
  }
  private def readHeapUsage() = RealProfiler.memoryMx.getHeapMemoryUsage.getUsed
}

private [profile] class RealProfiler(reporter : ProfileReporter)(using Context) extends Profiler with NotificationListener {
  val id: Int = RealProfiler.idGen.incrementAndGet()
  private val mainThread = Thread.currentThread()
  private val gcEvents = UnrolledBuffer[GcEventData]()
  private var nextAfterUnitSnap: Long = System.nanoTime()

  private final val GcThreadId = "GC"

  enum Category:
    def name: String = this.toString().toLowerCase()
    case Run, Phase, File, TypeCheck, Implicit, Macro, Completion
  private [profile] val chromeTrace =
    if ctx.settings.YprofileTrace.isDefault
    then null
    else
      val filename = ctx.settings.YprofileTrace.value
      // Compilation units requiring multi-stage compilation (macros) would create a new profiler instances
      // We need to store the traces in the seperate file to prevent overriding its content.
      // Alternatives: sharing ChromeTrace instance between all runs / manual concatation after all runs are done
      // FIXME: The first assigned runId is equal to 2 instead of 1 (InitialRunId).
      // Fix me when bug described in Compiler.runId is resolved by removing +/- 1 adjustments
      val suffix = if ctx.runId > InitialRunId + 1 then s".${ctx.runId - 1}" else ""
      ChromeTrace(Paths.get(s"$filename$suffix"))

  private val compilerRunEvent: TracedEventId = traceDurationStart(Category.Run, s"scalac-$id")

  def completeBackground(threadRange: ProfileRange): Unit =
    reporter.reportBackground(this, threadRange)

  def outDir: AbstractFile = ctx.settings.outputDir.value

  @nowarn
  private def doGC(): Unit = {
    System.gc()
    System.runFinalization()
  }

  RealProfiler.gcMx foreach {
    case emitter: NotificationEmitter => emitter.addNotificationListener(this, null, null)
    case gc => println(s"Cant connect gcListener to ${gc.getClass}")
  }

  reporter.header(this)

  override def finished(): Unit = {
    //we may miss a GC event if gc is occurring as we call this
    RealProfiler.gcMx foreach {
      case emitter: NotificationEmitter => emitter.removeNotificationListener(this)
      case gc =>
    }
    reporter.close(this)
    if chromeTrace != null then
      traceDurationEnd(Category.Run, compilerRunEvent)
      for gcEvent <- gcEvents
      do {
        val durationNanos = TimeUnit.MILLISECONDS.toNanos(gcEvent.durationMillis)
        val startNanos = gcEvent.endNanos - durationNanos
        chromeTrace.traceDurationEvent(gcEvent.name, startNanos, durationNanos, tid = GcThreadId)
      }
      chromeTrace.close()
  }


  override def handleNotification(notification: Notification, handback: scala.Any): Unit = {
    import java.lang.{Long => jLong}
    import java.lang.{Integer => jInt}
    val reportNs = System.nanoTime()
    val data = notification.getUserData
    val tpe = notification.getType
    data match {
      case cd: CompositeData if tpe == "com.sun.management.gc.notification" =>
        val name = cd.get("gcName").toString
        val action = cd.get("gcAction").toString
        val cause = cd.get("gcCause").toString
        val info = cd.get("gcInfo").asInstanceOf[CompositeData]
        val duration = info.get("duration").asInstanceOf[jLong].longValue()
        val startTime = info.get("startTime").asInstanceOf[jLong].longValue()
        val endTime = info.get("endTime").asInstanceOf[jLong].longValue()
        val threads = info.get("GcThreadCount").asInstanceOf[jInt].longValue()
        val gcEvent = GcEventData("", reportNs, startTime, endTime, duration, name, action, cause, threads)
        synchronized { gcEvents += gcEvent }
        reporter.reportGc(gcEvent)
    }
  }

  override def afterPhase(event: TracedEventId, phase: Phase, snapBefore: ProfileSnap): Unit = {
    assert(mainThread eq Thread.currentThread())
    val initialSnap = RealProfiler.snapThread(0)
    if (ctx.settings.YprofileExternalTool.value.contains(phase.toString)) {
      println("Profile hook stop")
      ExternalToolHook.after()
    }
    val finalSnap = if (ctx.settings.YprofileRunGcBetweenPhases.value.contains(phase.toString)) {
      doGC()
      initialSnap.updateHeap(RealProfiler.readHeapUsage())
    }
    else initialSnap
    traceDurationEnd(Category.Phase, event)
    traceThreadSnapshotCounters()
    reporter.reportForeground(this, ProfileRange(snapBefore, finalSnap, phase, "", 0, Thread.currentThread))
  }

  override def beforePhase(phase: Phase): (TracedEventId, ProfileSnap) = {
    assert(mainThread eq Thread.currentThread())
    traceThreadSnapshotCounters()
    val eventId = traceDurationStart(Category.Phase, phase.phaseName)
    if (ctx.settings.YprofileRunGcBetweenPhases.value.contains(phase.toString))
      doGC()
    if (ctx.settings.YprofileExternalTool.value.contains(phase.toString)) {
      println("Profile hook start")
      ExternalToolHook.before()
    }
    (eventId, RealProfiler.snapThread(0))
  }

  override def beforeUnit(phase: Phase, unit: CompilationUnit): TracedEventId = {
    assert(mainThread eq Thread.currentThread())
    if chromeTrace != null then
      traceThreadSnapshotCounters()
      traceDurationStart(Category.File, unit.source.name)
    else TracedEventId.Empty
  }

  override def afterUnit(event: TracedEventId): Unit = {
    assert(mainThread eq Thread.currentThread())
    if chromeTrace != null then
      traceDurationEnd(Category.File, event)
      traceThreadSnapshotCounters()
  }

  private def traceThreadSnapshotCounters(initialSnap: => ProfileSnap = RealProfiler.snapThread(0)) =
    if chromeTrace != null && System.nanoTime() > nextAfterUnitSnap then {
      val snap = initialSnap
      chromeTrace.traceCounterEvent("allocBytes", "allocBytes", snap.allocatedBytes, processWide = false)
      chromeTrace.traceCounterEvent("heapBytes", "heapBytes", snap.heapBytes, processWide = true)
      chromeTrace.traceCounterEvent("classesLoaded", "classesLoaded", snap.totalClassesLoaded, processWide = true)
      chromeTrace.traceCounterEvent("jitCompilationTime", "jitCompilationTime", snap.totalJITCompilationTime, processWide = true)
      chromeTrace.traceCounterEvent("userTime", "userTime", snap.userTimeNanos, processWide = false)
      chromeTrace.traceCounterEvent("cpuTime", "cpuTime", snap.cpuTimeNanos, processWide = false)
      chromeTrace.traceCounterEvent("idleTime", "idleTime", snap.idleTimeNanos, processWide = false)
      nextAfterUnitSnap = System.nanoTime() + 10 * 1000 * 1000
    }

  override def beforeTypedDef(sym: Symbol): TracedEventId = traceDurationStart(Category.TypeCheck, symbolName(sym))
  override def afterTypedDef(event: TracedEventId): Unit = traceDurationEnd(Category.TypeCheck, event)

  override def beforeImplicitSearch(pt: Type): TracedEventId = traceDurationStart(Category.Implicit, s"?[${symbolName(pt.typeSymbol)}]", colour = "yellow")
  override def afterImplicitSearch(event: TracedEventId): Unit = traceDurationEnd(Category.Implicit, event, colour = "yellow")

  override def beforeMacroSplice(macroSym: Symbol): TracedEventId = traceDurationStart(Category.Macro, s"«${symbolName(macroSym)}»", colour = "olive")
  override def afterMacroSplice(event: TracedEventId): Unit = traceDurationEnd(Category.Macro, event, colour = "olive")

  override def beforeCompletion(root: Symbol, associatedFile: => AbstractFile): (TracedEventId, String) =
    if chromeTrace == null
    then EmptyCompletionEvent
    else
      val completionName = this.completionName(root, associatedFile)
      val event = TracedEventId(associatedFile.name)
      chromeTrace.traceDurationEventStart(Category.Completion.name, "↯", colour = "thread_state_sleeping")
      chromeTrace.traceDurationEventStart(Category.File.name, event)
      chromeTrace.traceDurationEventStart(Category.Completion.name, completionName)
      (event, completionName)

  override def afterCompletion(event: TracedEventId, completionName: String): Unit =
    if chromeTrace != null
    then
      chromeTrace.traceDurationEventEnd(Category.Completion.name, completionName)
      chromeTrace.traceDurationEventEnd(Category.File.name, event)
      chromeTrace.traceDurationEventEnd(Category.Completion.name, "↯", colour = "thread_state_sleeping")

  private inline def traceDurationStart(category: Category, inline eventName: String, colour: String = ""): TracedEventId =
    if chromeTrace == null
    then TracedEventId.Empty
    else
      val event = TracedEventId(eventName)
      chromeTrace.traceDurationEventStart(category.name, event, colour)
      event

  private inline def traceDurationEnd(category: Category, event: TracedEventId, colour: String = ""): Unit =
    if chromeTrace != null then
      chromeTrace.traceDurationEventEnd(category.name, event, colour)

  private def symbolName(sym: Symbol): String = s"${sym.showKind} ${sym.showName}"
  private def completionName(root: Symbol, associatedFile: AbstractFile): String =
    def isTopLevel = root.owner != NoSymbol && root.owner.is(Flags.Package)
    if root.is(Flags.Package) || isTopLevel
    then root.javaBinaryName
    else
      val enclosing = root.enclosingClass
      s"${enclosing.javaBinaryName}::${root.name}"
}

enum EventType(name: String):
  // main thread with other tasks
  case MAIN extends EventType("main")
  // other task ( background thread)
  case BACKGROUND extends EventType("background")
  // total for compile
  case GC extends EventType("GC")

sealed trait ProfileReporter {
  def reportBackground(profiler: RealProfiler, threadRange: ProfileRange): Unit
  def reportForeground(profiler: RealProfiler, threadRange: ProfileRange): Unit

  def reportGc(data: GcEventData): Unit

  def header(profiler: RealProfiler) :Unit
  def close(profiler: RealProfiler) :Unit
}

object ConsoleProfileReporter extends ProfileReporter {
  @sharable var totalAlloc = 0L

  override def reportBackground(profiler: RealProfiler, threadRange: ProfileRange): Unit =
    reportCommon(EventType.BACKGROUND, profiler, threadRange)
  override def reportForeground(profiler: RealProfiler, threadRange: ProfileRange): Unit =
    reportCommon(EventType.MAIN, profiler, threadRange)
  @nowarn("cat=deprecation")
  private def reportCommon(tpe:EventType, profiler: RealProfiler, threadRange: ProfileRange): Unit =
    totalAlloc += threadRange.allocatedBytes
    println(s"${threadRange.phase.phaseName.replace(',', ' ')},run ns = ${threadRange.runNs},idle ns = ${threadRange.idleNs},cpu ns = ${threadRange.cpuNs},user ns = ${threadRange.userNs},allocated = ${threadRange.allocatedBytes},heap at end = ${threadRange.end.heapBytes}, total allocated = $totalAlloc ")

  override def close(profiler: RealProfiler): Unit = ()

  override def header(profiler: RealProfiler): Unit =
    println(s"Profiler start (${profiler.id}) ${profiler.outDir}")

  override def reportGc(data: GcEventData): Unit =
    println(s"Profiler GC reported ${data.gcEndMillis - data.gcStartMillis}ms")
}

class StreamProfileReporter(out:PrintWriter) extends ProfileReporter {
  override def header(profiler: RealProfiler): Unit = {
    out.println(s"info, ${profiler.id}, version, 2, output, ${profiler.outDir}")
    out.println(s"header(main/background),startNs,endNs,runId,phaseId,phaseName,purpose,task-count,threadId,threadName,runNs,idleNs,cpuTimeNs,userTimeNs,allocatedByte,heapSize")
    out.println(s"header(GC),startNs,endNs,startMs,endMs,name,action,cause,threads")
  }

  override def reportBackground(profiler: RealProfiler, threadRange: ProfileRange): Unit =
    reportCommon(EventType.BACKGROUND, profiler, threadRange)
  override def reportForeground(profiler: RealProfiler, threadRange: ProfileRange): Unit =
    reportCommon(EventType.MAIN, profiler, threadRange)
  @nowarn("cat=deprecation")
  private def reportCommon(tpe:EventType, profiler: RealProfiler, threadRange: ProfileRange): Unit =
    out.println(s"$tpe,${threadRange.start.snapTimeNanos},${threadRange.end.snapTimeNanos},${profiler.id},${threadRange.phase.id},${threadRange.phase.phaseName.replace(',', ' ')},${threadRange.purpose},${threadRange.taskCount},${threadRange.thread.getId},${threadRange.thread.getName},${threadRange.runNs},${threadRange.idleNs},${threadRange.cpuNs},${threadRange.userNs},${threadRange.allocatedBytes},${threadRange.end.heapBytes} ")

  override def reportGc(data: GcEventData): Unit = {
    val duration = TimeUnit.MILLISECONDS.toNanos(data.gcEndMillis - data.gcStartMillis + 1)
    val start = data.reportTimeNs - duration
    out.println(s"${EventType.GC},$start,${data.reportTimeNs},${data.gcStartMillis}, ${data.gcEndMillis},${data.name},${data.action},${data.cause},${data.threads}")
  }

  override def close(profiler: RealProfiler): Unit = {
    out.flush()
    out.close()
  }
}
