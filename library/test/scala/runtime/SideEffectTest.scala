package scala.runtime

import org.junit.Assert.*

trait SideEffectTest {

  private var affected = false

  def sideEffect() = {
    assertFalse(affected)
    affected = true
  }

  def checkSideEffected(): Unit = {
    assertTrue(affected)
  }

}
