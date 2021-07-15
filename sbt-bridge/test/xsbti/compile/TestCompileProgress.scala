package xsbti.compile

import org.junit.Assert.*

import scala.collection.mutable

class TestCompileProgress extends CompileProgress {

  val startUnitCalls: mutable.Buffer[(String, String)] = mutable.Buffer.empty[(String, String)]

  override def startUnit(phase: String, unitPath: String): Unit =
    startUnitCalls += ((phase, unitPath))

  // TODO: we should also call advance with calculated current / total
  //  Beware: scala3-compiler uses compiler-interface 1.3.5 which is binary incompatible
  //  with the one used un scala3-sbt-bridge (1.4.5)
  //  it has different signature def advance(current: Int, total: Int): Boolean
  override def advance(current: Int, total: Int, prevPhase: String, nextPhase: String): Boolean =
    fail("'CompileProgress.advance' method is not supposed to be called at this moment (not yet implemented)").asInstanceOf[Nothing]
}
