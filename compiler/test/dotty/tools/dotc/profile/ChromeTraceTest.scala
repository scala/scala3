package dotty.tools.dotc.profile

import java.io.*

import org.junit.Assert.*
import org.junit.*
import java.nio.file.Files
import java.nio.charset.StandardCharsets
import java.util.concurrent.locks.LockSupport
import scala.concurrent.duration.*

class ChromeTraceTest:
  private def testTraceOutputs(generator: ChromeTrace => Unit)(checkContent: PartialFunction[List[String], Unit]): Unit = {
    val outfile = Files.createTempFile("trace-", ".json").nn
    val tracer = new ChromeTrace(outfile)
    try generator(tracer)
    finally tracer.close()
    val contentLines = scala.io.Source.fromFile(outfile.toFile().nn).getLines().toList
    checkContent.applyOrElse(contentLines, content => fail(s"Invalid output lines: ${content.mkString(System.lineSeparator())}"))
  }

  @Test def traceCounterEvent(): Unit = testTraceOutputs{ tracer =>
    tracer.traceCounterEvent("foo", "counter1", 42, processWide = true)
    tracer.traceCounterEvent("bar", "counter2", 21, processWide = false)
  }{
    case """{"traceEvents":[""" ::
        s"""{"cat":"scalac","name":"foo","ph":"C","tid":"${tid1}","pid":"${pid1}","ts":${ts1},"args":{"counter1":42}}""" ::
        s""",{"cat":"scalac","name":"bar","ph":"C","tid":"${tid2}","pid":"${pid2}","ts":${ts2},"args":{"counter2":21}}""" ::
           "]}" :: Nil =>
      assertEquals(tid1, tid2)
      assertTrue(tid1.toIntOption.isDefined)
      assertNotEquals(pid1, pid2)
      assertTrue(pid1.toIntOption.isDefined)
      assertEquals(s"$pid1-$tid1", pid2)
      assertTrue(ts1.toLong < ts2.toLong)
  }

  @Test def traceDurationEvent(): Unit = testTraceOutputs{ tracer =>
    tracer.traceDurationEvent(name = "name1", startNanos = 1000L, durationNanos = 2500L,  tid = "this-thread")
    tracer.traceDurationEvent(name = "name2", startNanos = 1000L, durationNanos = 5000L,  tid = "this-thread", pidSuffix = "pidSuffix")
  }{
    case """{"traceEvents":[""" ::
        s"""{"cat":"scalac","name":"name1","ph":"X","tid":"this-thread","pid":"${pid1}","ts":1,"dur":2}""" ::
        s""",{"cat":"scalac","name":"name2","ph":"X","tid":"this-thread","pid":"${pid2}","ts":1,"dur":5}""" ::
           "]}" :: Nil =>
      assertTrue(pid1.toIntOption.isDefined)
      assertEquals(s"$pid1-pidSuffix", pid2)
  }

  @Test def traceDurationEvents(): Unit = {
    val testStart = System.nanoTime()
    testTraceOutputs{ tracer =>
    tracer.traceDurationEventStart(cat = "test1", name = "event1")
    LockSupport.parkNanos(2.millis.toNanos)
    tracer.traceDurationEventStart(cat = "test2", name = "event2", colour = "RED", pidSuffix = "pid-suffix")
    LockSupport.parkNanos(4.millis.toNanos)
    tracer.traceDurationEventEnd(cat = "test2", name = "event2")
    LockSupport.parkNanos(8.millis.toNanos)
    tracer.traceDurationEventEnd(cat = "test1", name = "event1", colour = "RED", pidSuffix = "pid-suffix")
  }{
    case """{"traceEvents":[""" ::
        s"""{"cat":"test1","name":"event1","ph":"B","pid":"${pid1}","tid":"${tid1}","ts":${ts1}}""" ::
        s""",{"cat":"test2","name":"event2","ph":"B","pid":"${pid2}","tid":"${tid2}","ts":${ts2},"cname":"RED"}""" ::
        s""",{"cat":"test2","name":"event2","ph":"E","pid":"${pid3}","tid":"${tid3}","ts":${ts3}}""" ::
        s""",{"cat":"test1","name":"event1","ph":"E","pid":"${pid4}","tid":"${tid4}","ts":${ts4},"cname":"RED"}""" ::
           "]}" :: Nil =>
      val traceEnd = System.nanoTime()
      assertTrue(tid1.toIntOption.isDefined)
      assertEquals(pid1, pid3)
      assertTrue(pid1.endsWith(s"-$tid1"))
      assertEquals(pid2, pid4)
      assertTrue(pid2.endsWith("-pid-suffix"))
      List(tid1, tid2, tid3).foreach: tid =>
        assertEquals(tid4, tid)
      List(pid1, pid2, pid3, pid4).foreach: pid =>
        assertTrue(pid.takeWhile(_ != '-').toIntOption.isDefined)

      List(ts1, ts2, ts3, ts4).map(_.toLong) match {
        case all @ List(ts1, ts2, ts3, ts4) =>
          all.foreach: ts =>
            // Timestamps are presented using Epoch microsecondos
            assertTrue(ts >= testStart / 1000)
            assertTrue(ts <= traceEnd / 1000)
          assertTrue(ts2 >= ts1 + 2.millis.toMicros)
          assertTrue(ts3 >= ts2 + 4.millis.toMicros)
          assertTrue(ts4 >= ts3 + 8.millis.toMicros)
        case _ => fail("unreachable")
      }
  }
}
